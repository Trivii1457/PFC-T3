package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.parallel.CollectionConverters._

@RunWith(classOf[JUnitRunner])
class TestMatriz extends AnyFunSuite {
  val matriz = new MatrixParallel()

  test("Test MultMatriz"){
    val A = matriz.MatrizAlAzar(4, 10)
    val B = matriz.multMatriz(A, A)
    assert(B.length == 4)
    assert(B(0).length == 4)
  }
  test("Test MultMatrizRec 2x2") {
    val A = matriz.MatrizAlAzar(2, 10)
    val B = matriz.MatrizAlAzar(2, 10)
    val C = matriz.MultMatrizRec(A, B)
    assert(C.length == 2)
    assert(C.forall(_.length == 2))
  }

  test("Test MultMatrizRec 8x8") {
    val D = matriz.MatrizAlAzar(8, 5)
    val E = matriz.MatrizAlAzar(8, 5)
    val F = matriz.MultMatrizRec(D, E)
    assert(F.length == 8)
    assert(F.forall(_.length == 8))
  }

  test("Test MultMatrizRec 4x4") {
    val G = matriz.MatrizAlAzar(4, 20)
    val H = matriz.MatrizAlAzar(4, 20)
    val I = matriz.MultMatrizRec(G, H)
    assert(I.length == 4)
    assert(I.forall(_.length == 4))
  }

  test("Test MultMatrizRecPar 2x2") {
    val A = matriz.MatrizAlAzar(2, 10)
    val B = matriz.MatrizAlAzar(2, 10)
    val C = matriz.MultMatrizRecPar(A, B)
    assert(C.length == 2)
    assert(C.forall(_.length == 2))
  }

  test("Test MultMatrizRecPar 8x8") {
    val D = matriz.MatrizAlAzar(8, 5)
    val E = matriz.MatrizAlAzar(8, 5)
    val F = matriz.MultMatrizRecPar(D, E)
    assert(F.length == 8)
    assert(F.forall(_.length == 8))
  }

  test("Test MultMatrizRecPar 4x4") {
    val G = matriz.MatrizAlAzar(4, 20)
    val H = matriz.MatrizAlAzar(4, 20)
    val I = matriz.MultMatrizRecPar(G, H)
    assert(I.length == 4)
    assert(I.forall(_.length == 4))
  }

  test("Test MultMatrizStrassen 2x2") {
    val A = matriz.MatrizAlAzar(2, 10)
    val B = matriz.MatrizAlAzar(2, 10)
    val C = matriz.MultMatrizStrassen(A, B)
    assert(C.length == 2)
    assert(C.forall(_.length == 2))
  }

  test("Test MultMatrizStrassen 4x4") {
    val D = matriz.MatrizAlAzar(4, 5)
    val E = matriz.MatrizAlAzar(4, 5)
    val F = matriz.MultMatrizStrassen(D, E)
    assert(F.length == 4)
    assert(F.forall(_.length == 4))
  }

  test("Test MultMatrizStrassen 8x8") {
    val G = matriz.MatrizAlAzar(8, 20)
    val H = matriz.MatrizAlAzar(8, 20)
    val I = matriz.MultMatrizStrassen(G, H)
    assert(I.length == 8)
    assert(I.forall(_.length == 8))
  }

  test("Test StrassenParallel 2x2") {
    val A = matriz.MatrizAlAzar(2, 10)
    val B = matriz.MatrizAlAzar(2, 10)
    val C = matriz.StrassenParallel(A, B)
    assert(C.length == 2)
    assert(C.forall(_.length == 2))
  }

  test("Test StrassenParallel 4x4") {
    val D = matriz.MatrizAlAzar(4, 5)
    val E = matriz.MatrizAlAzar(4, 5)
    val F = matriz.StrassenParallel(D, E)
    assert(F.length == 4)
    assert(F.forall(_.length == 4))
  }

  test("Test StrassenParallel 8x8") {
    val G = matriz.MatrizAlAzar(8, 20)
    val H = matriz.MatrizAlAzar(8, 20)
    val I = matriz.StrassenParallel(G, H)
    assert(I.length == 8)
    assert(I.forall(_.length == 8))
  }

  test("Test ProductoPunto 1") {
    val t1 = Vector(1, 2, 3)
    val u1 = Vector(4, 5, 6)
    val result1 = matriz.ProductoPunto(t1, u1)
    assert(result1 == 32)
  }

  test("Test ProductoPunto 2") {
    val t2 = Vector(2, 3, 4)
    val u2 = Vector(1, 0, -1)
    val result2 = matriz.ProductoPunto(t2, u2)
    assert(result2 == -2)
  }

  test("Test ProductoPunto 3") {
    val t3 = Vector(0, 0, 0)
    val u3 = Vector(1, 2, 3)
    val result3 = matriz.ProductoPunto(t3, u3)
    assert(result3 == 0)
  }

  test("Test prodPuntoParD 1") {
    val t1 = Vector(1, 2, 3).par
    val u1 = Vector(4, 5, 6).par
    val result1 = matriz.prodPuntoParD(t1, u1)
    assert(result1 == 32)
  }

  test("Test prodPuntoParD 2") {
    val t2 = Vector(2, 3, 4).par
    val u2 = Vector(1, 0, -1).par
    val result2 = matriz.prodPuntoParD(t2, u2)
    assert(result2 == -2)
  }

  test("Test prodPuntoParD 3") {
    val t3 = Vector(0, 0, 0).par
    val u3 = Vector(1, 2, 3).par
    val result3 = matriz.prodPuntoParD(t3, u3)
    assert(result3 == 0)
  }

}
