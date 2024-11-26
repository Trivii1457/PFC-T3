/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package taller

object App {
  def main(args: Array[String]): Unit = {
    println(greeting())

    val matriz = new MatrixParallel()

    val A = matriz.MatrizAlAzar(3, 10) // Matriz de 3x3 con valores aleatorios entre 0 y 10
    println(A)

    val B = matriz.multMatriz(A, A)
    println(B)

    val C = matriz.SubMatriz(B, 0, 0, 2)
    println(C)

    val D = matriz.mulMatrizPar(A, A)
    println(D)

    val E = matriz.ResMatriz(A, A)
    println(E)
  }

  def greeting(): String = "Hello, world!"
}
