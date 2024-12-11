package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.parallel.CollectionConverters._

@RunWith(classOf[JUnitRunner])
class TestMatriz extends AnyFunSuite {
  val matriz = new MatrixParallel()

  //________________Test MultMatriz________________

    test("Test MultMatriz 2x2") {
      val A = matriz.MatrizAlAzar(2, 10)
      val B = matriz.multMatriz(A,A)
      assert(B.length == 2)
      assert(B(0).length == 2)
    }
    test("Test MultMatriz 4x4") {
    val A = matriz.MatrizAlAzar(4, 10)
    val B = matriz.multMatriz(A,A)
    assert(B.length == 4)
    assert(B(0).length == 4)
    }
    test("Test MultMatriz 8x8") {
      val A = matriz.MatrizAlAzar(8, 10)
      val B = matriz.multMatriz(A,A)
      assert(B.length == 8)
      assert(B(0).length == 8)
    }

    test("Test MultMatriz 16x16") {
      val A = matriz.MatrizAlAzar(16, 10)
      val B = matriz.multMatriz(A,A)
      assert(B.length == 16)
      assert(B(0).length == 16)
    }

    test("Test MultMatriz 32x32") {
      val A = matriz.MatrizAlAzar(32, 10)
      val B = matriz.multMatriz(A,A)
      assert(B.length == 32)
      assert(B(0).length == 32)
    }


  //________________Tests MultMatrizRec________________

    test("Test MultMatrizRec 2x2") {
      val A = matriz.MatrizAlAzar(2, 10)
      val B = matriz.MatrizAlAzar(2, 10)
      val C = matriz.MultMatrizRec(A, B)
      assert(C.length == 2)
      assert(C.forall(_.length == 2))
    }

    test("Test MultMatrizRec 4x4") {
      val G = matriz.MatrizAlAzar(4, 20)
      val H = matriz.MatrizAlAzar(4, 20)
      val I = matriz.MultMatrizRec(G, H)
      assert(I.length == 4)
      assert(I.forall(_.length == 4))
    }

    test("Test MultMatrizRec 8x8") {
      val D = matriz.MatrizAlAzar(8, 5)
      val E = matriz.MatrizAlAzar(8, 5)
      val F = matriz.MultMatrizRec(D, E)
      assert(F.length == 8)
      assert(F.forall(_.length == 8))
    }

    test("Test MultMatrizRec 16x16"){
      val J = matriz.MatrizAlAzar(16,10)
      val K = matriz.MatrizAlAzar(16,10)
      val L = matriz.MultMatrizRec(J, K)
      assert(L.length == 16)
      assert(L.forall(_.length == 16))
    }

    test("Test MultMatrizRec 32x32"){
      val M = matriz.MatrizAlAzar(32,15)
      val N = matriz.MatrizAlAzar(32,15)
      val O = matriz.MultMatrizRec(M,N)
      assert(O.length == 32)
      assert(O.forall(_.length == 32))
    }

  //____________Tests MultMatrizRecPar_____________

    test("Test MultMatrizRecPar 2x2") {
      val A = matriz.MatrizAlAzar(2, 10)
      val B = matriz.MatrizAlAzar(2, 10)
      val C = matriz.MultMatrizRecPar(A, B)
      assert(C.length == 2)
      assert(C.forall(_.length == 2))
    }

    test("Test MultMatrizRecPar 4x4") {
      val G = matriz.MatrizAlAzar(4, 20)
      val H = matriz.MatrizAlAzar(4, 20)
      val I = matriz.MultMatrizRecPar(G, H)
      assert(I.length == 4)
      assert(I.forall(_.length == 4))
    }

    test("Test MultMatrizRecPar 8x8") {
      val D = matriz.MatrizAlAzar(8, 5)
      val E = matriz.MatrizAlAzar(8, 5)
      val F = matriz.MultMatrizRecPar(D, E)
      assert(F.length == 8)
      assert(F.forall(_.length == 8))
    }

    test("Test MultMatrizRecPar 16x16"){
      val J = matriz.MatrizAlAzar(16,10)
      val K = matriz.MatrizAlAzar(16,10)
      val L = matriz.MultMatrizRecPar(J, K)
      assert(L.length == 16)
      assert(L.forall(_.length == 16))
    }

    test("Test MultMatrizRecPar 32x32"){
      val M = matriz.MatrizAlAzar(32, 15)
      val N = matriz.MatrizAlAzar(32,15)
      val O = matriz.MultMatrizRecPar(M, N)
      assert(O.length == 32)
      assert(O.forall(_.length == 32))
    }

  //____________Test Strassen__________________
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

    test("Tes MultMatrizStrassen 16x16"){
      val J = matriz.MatrizAlAzar(16,10)
      val K = matriz.MatrizAlAzar(16,10)
      val L = matriz.MultMatrizStrassen(J, K)
      assert(L.length == 16)
      assert(L.forall(_.length == 16))
    }

    test("Tes MultMatrizStrassen 32x32"){
      val M = matriz.MatrizAlAzar(32,15)
      val N = matriz.MatrizAlAzar(32,15)
      val O = matriz.MultMatrizStrassen(M, N)
      assert(O.length == 32)
      assert(O.forall(_.length == 32))
    }

  //__________Test StrassenParallel_______________
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

    test("Test StrassenParallel 16x16") {
      val J = matriz.MatrizAlAzar(16, 10)
      val K = matriz.MatrizAlAzar(16, 10)
      val L = matriz.StrassenParallel(J, K)
      assert(L.length == 16)
      assert(L.forall(_.length == 16))
    }

    test("Test StrassenParallel 32x32") {
      val M = matriz.MatrizAlAzar(32, 10)
      val N = matriz.MatrizAlAzar(32, 10)
      val O = matriz.StrassenParallel(M, N)
      assert(O.length == 32)
      assert(O.forall(_.length == 32))
    }

  //__________Test ProductoPunto________________
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

    test("Test ProductoPunto 4") {
      val t4 = Vector(10, 20, 30)
      val u4 = Vector(1, -1, 1)
      val result4 = matriz.ProductoPunto(t4, u4)
      assert(result4 == 20)
    }

    test("Test ProductoPunto 5") {
      val t5 = Vector(-5, -10, -15)
      val u5 = Vector(-2, -3, -4)
      val result5 = matriz.ProductoPunto(t5, u5)
      assert(result5 == 100)
    }

    //_________TestProductoPuntoPar_____________

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

    test("Test prodPuntoParD 4") {
      val t4 = Vector(10, 20, 30).par
      val u4 = Vector(1, -1, 1).par
      val result4 = matriz.prodPuntoParD(t4, u4)
      assert(result4 == 20)
    }

    test("Test prodPuntoParD 5") {
      val t5 = Vector(-5, -10, -15).par
      val u5 = Vector(-2, -3, -4).par
      val result5 = matriz.prodPuntoParD(t5, u5)
      assert(result5 == 100)
    }

}
