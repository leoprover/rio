package net.aurelee.rio

import net.aurelee.rio.core.{CNF, Formula}
import net.aurelee.rio.core.{PLNeg, PLProp}
import net.aurelee.rio.core.{cnfFormulaToMultiset, mkConjs, mkImpl, mkNeg}

import scala.annotation.unused
import scala.collection.mutable
import scala.sys.process.ProcessLogger

package object sat {
  implicit lazy val globalPicoSATInstance: PicoSAT = PicoSAT.get()
  final lazy val musToolFile: String = {
    val must = getClass.getResourceAsStream("/must")
    if (must == null) throw new IllegalStateException("Missing MUST resource '/must'; build and package the MUS backend first")
    val target = java.nio.file.Files.createTempFile("riomust", "")
    target.toFile.deleteOnExit()
    try java.nio.file.Files.copy(must, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    finally must.close()
    if (!target.toFile.setExecutable(true)) throw new IllegalStateException("Cannot make the MUST resource executable")
    target.toAbsolutePath.toString
  }

  final def cnfToDimacsClauses(multisetCNF: CNF): (Seq[Seq[Int]], Map[String, Int]) = {
    //    println(s"cnfToDimacsClause of cnf = ${cnf.pretty}")
    val translatedClauses: mutable.ListBuffer[Seq[Int]] = mutable.ListBuffer.empty
    val namingMap: mutable.Map[String, Int] = mutable.Map.empty

    @inline def translateLiteral(literalName: String): Int = { // translate names to integers 0 ... k
      namingMap.get(literalName) match {
        case Some(value) => value
        case None =>
          val newValue = namingMap.size
          namingMap.addOne(literalName -> newValue)
          newValue
      }
    }

    multisetCNF.foreach { clause => // iterate over clauses
      val translatedLits = clause.map { literal =>
        (literal: @unchecked) match {
          case PLNeg(PLProp(name)) => // Negated because in dimacs negative literals are negative
            val value = translateLiteral(name)
            -(value + 1) // +1 because in dimacs we cannot use 0 as literal name
          case PLProp(name) =>
            val value = translateLiteral(name)
            value + 1 // see above
        }
      }
      translatedClauses.append(translatedLits)
    }
    (translatedClauses.toSeq, namingMap.toMap)
  }

  final def cnfToDimacsProblemString(multisetCNF: CNF): (String, Map[String, Int]) = {
    //    println(s"cnfToDimacsProblem cnf = ${cnf.pretty}")
    val sb: mutable.StringBuilder = new mutable.StringBuilder()
    val (dimacsClauses, symbolnameMap) = cnfToDimacsClauses(multisetCNF)
    val varCount = symbolnameMap.size
    val clauseCount = dimacsClauses.size
    sb.append(s"p cnf $varCount $clauseCount\n")
    dimacsClauses foreach { clause =>
      sb.append(clause.mkString(" "))
      sb.append(s" 0\n")
    }
    (sb.init.toString /*drop last newline*/, symbolnameMap)
  }

  final def satisfiable(formula: Formula)(implicit solver: PicoSAT): Boolean = {
    solveInternal(formula)(solver)
    solver.state == PicoSAT.SAT
  }
  final def unsatisfiable(formula: Formula)(implicit solver: PicoSAT): Boolean = {
    solveInternal(formula)(solver)
    solver.state == PicoSAT.UNSAT
  }
  final def consistent(formulas: Iterable[Formula])(implicit solver: PicoSAT): Boolean = {
    val conjunct = mkConjs(formulas)
    satisfiable(conjunct)(solver)
  }
  final def consequence(assumptions: Iterable[Formula], conjecture: Formula)(implicit solver: PicoSAT): Boolean = {
    val formulas = Iterable.concat(assumptions, Seq(mkNeg(conjecture)))
    unsatisfiable(mkConjs(formulas))(solver)
  }
  final def tautology(formula: Formula)(implicit solver: PicoSAT): Boolean = {
    unsatisfiable(mkNeg(formula))(solver)
  }
  @unused
  final def equivalent(f: Formula, g: Formula)(implicit solver: PicoSAT): Boolean = {
    val impl = mkImpl(f, g)
    val lpmi = mkImpl(g, f)
    tautology(mkConjs(Seq(impl, lpmi)))(solver)
  }
  private[this] final def solveInternal(formula: Formula)(solver: PicoSAT): Unit = {
    import net.aurelee.rio.core.cnf
    val clausified = cnf(formula)
    val multisetCNF = cnfFormulaToMultiset(clausified)
//    println(s"[PicoSAT] Check ${simplified.pretty}")
    val (picoSatInput, _) = cnfToDimacsClauses(multisetCNF)
    solver.reset()
    picoSatInput.foreach(solver.addClause)
    requireDecided(solver.solve())
  }

  private[sat] final def requireDecided(state: PicoSAT.State): Unit = state match {
    case PicoSAT.SAT | PicoSAT.UNSAT => ()
    case PicoSAT.Unknown => throw new IllegalStateException("PicoSAT returned UNKNOWN; no logical answer is available")
  }

  final def allMUSes(multisetCNF: CNF)(implicit solver: PicoSAT): Seq[CNF] = {
    // A satisfiable clause set has no MUS, including the empty CNF.
    if (consistent(multisetCNF.map(net.aurelee.rio.core.mkDisjs))(solver)) return Seq.empty
    // translate to Dimacs
    val (dimacs, symbolMap) = cnfToDimacsProblemString(multisetCNF)
    val reverseMap = symbolMap.map(_.swap) // works because symbolMap is injective
    // run MUS enumeration tool
    val muses = runMUST(dimacs)
    // translate result file to Seq[Formula]
    val result: Seq[CNF] = muses.map { mus =>
        mus.map { clause =>
          clause.map { lit =>
          (lit: @unchecked) match {
            case i if i > 0 => PLProp(reverseMap(i - 1))
            case i if i < 0 => PLNeg(PLProp(reverseMap(-i - 1)))
          }
        }
      }
    }
    if (result.isEmpty) throw new IllegalStateException("MUST returned no MUS for an unsatisfiable input")
    val inputClauses = multisetCNF.map(_.toSet).toSet
    result.foreach { mus =>
      if (!mus.forall(clause => inputClauses(clause.toSet)))
        throw new IllegalStateException("MUST returned a clause outside its input")
      if (consistent(mus.map(net.aurelee.rio.core.mkDisjs))(solver))
        throw new IllegalStateException("MUST returned a satisfiable subset")
    }
    result
  }
  private[sat] final def runMUST(dimacs: String, executable: String = musToolFile): Seq[Seq[Seq[Int]]] = {
    import java.nio.file.Files
    val tempDimacsFile = Files.createTempFile("rio", ".cnf")
    try {
      Files.writeString(tempDimacsFile, dimacs)
      val tempOutputFile = Files.createTempFile("rio", ".musoutput")
      try {
        val markers = mutable.ArrayBuffer.empty[String]
        val errors = new StringBuilder
        val process = scala.sys.process.Process(Seq(executable, tempDimacsFile.toString, "-o", tempOutputFile.toString))
        val exitCode = process ! ProcessLogger(line => {
          val value = line.trim
          if (value == "Enumeration completed" || value.startsWith("Number of MUSes:")) markers += value
        }, line => if (errors.length < 4096) errors.append(line.take(4096 - errors.length)).append('\n'))
        if (exitCode != 0) throw new IllegalStateException(s"MUST failed with exit code $exitCode: $errors")
        val countPattern = "Number of MUSes: ([0-9]+)".r
        val counts = markers.collect { case countPattern(n) => parseMUSInteger(n) }
        if (markers.count(_ == "Enumeration completed") != 1 || counts.size != 1)
          throw new IllegalStateException(s"MUST did not report completed enumeration: $errors")
        val result = MUSesFromDimacs(Files.readString(tempOutputFile))
        if (result.size != counts.head)
          throw new IllegalStateException("MUST output count does not match its completion report")
        result
      } finally Files.deleteIfExists(tempOutputFile)
    } finally Files.deleteIfExists(tempDimacsFile)
  }
  private[sat] final def MUSesFromDimacs(musDimacs: String): Seq[Seq[Seq[Int]]] = {
    val lines = musDimacs.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
    val result = Vector.newBuilder[Seq[Seq[Int]]]
    val musHeader = "MUS #([0-9]+)".r
    val cnfHeader = "p\\s+cnf\\s+([0-9]+)\\s+([0-9]+)".r
    var position = 0
    var expectedIndex = 1
    while (position < lines.size) {
      lines(position) match {
        case musHeader(n) if parseMUSInteger(n) == expectedIndex => ()
        case _ => throw new IllegalStateException("Malformed MUST MUS header")
      }
      position += 1
      if (position >= lines.size) throw new IllegalStateException("Missing MUST CNF header")
      val (variables, clauses) = lines(position) match {
        case cnfHeader(v, c) => (parseMUSInteger(v), parseMUSInteger(c))
        case _ => throw new IllegalStateException("Malformed MUST CNF header")
      }
      position += 1
      if (clauses <= 0 || clauses > lines.size - position)
        throw new IllegalStateException("Missing MUST clauses or empty MUS")
      val mus = Vector.newBuilder[Seq[Int]]
      for (_ <- 0 until clauses) {
        val values = lines(position).split("\\s+").toVector.map(parseMUSInteger)
        if (values.isEmpty || values.last != 0 || values.init.exists(n => n == 0 || math.abs(n.toLong) > variables))
          throw new IllegalStateException("Malformed MUST clause or literal outside declared range")
        mus += values.init
        position += 1
      }
      result += mus.result()
      expectedIndex += 1
    }
    result.result()
  }

  private[this] def parseMUSInteger(value: String): Int =
    try value.toInt catch {
      case _: NumberFormatException => throw new IllegalStateException(s"Invalid integer in MUST output: $value")
    }

}
