package taller

import org.scalatest.funsuite.AnyFunSuite
class TestMatriz extends AnyFunSuite {
  val matriz = new MatrixParallel()

  test("Test MultMatriz"){
    val A = matriz.MatrizAlAzar(4, 10)
    val B = matriz.multMatriz(A, A)
    assert(B.length == 4)
    assert(B(0).length == 4)
  }

}
