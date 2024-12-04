/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package taller
import Math.pow

object App {
  def main(args: Array[String]): Unit = {
    println(greeting())

    val matriz = new MatrixParallel()

    val A = matriz.MatrizAlAzar(4, 10) // Matriz de 4x4 con valores aleatorios entre 0 y 10
    println(A)

    val B = matriz.multMatriz(A, A)
    println(B)

    val C = matriz.SubMatriz(B, 0, 0, 2)
    println(C)

    val D = matriz.mulMatrizPar(A, A)
    println(D)

    val E = matriz.ResMatriz(A, A)
    println(E)

    val F = matriz.MultMatrizRec(A, A)
    println(F)

    val G = matriz.MultMatrizRecPar(A, A)
    println(G)

    val H = matriz.MultMatrizStrassen(A, A)
    println(H)

    val I = matriz.StrassenParallel(A, A)
    println(I)
    println("-------------------"*5)
    println("Benchmarking")
    benchmarking()
  }

  def benchmarking(): Unit = {
    val Bench = new Benchmark()
    val matriz = new MatrixParallel()
    for (i <- 1 to 6) {
      val A = matriz.MatrizAlAzar(math.pow(2,i).toInt, 2)
      val B = matriz.MatrizAlAzar(math.pow(2,i).toInt, 2)
      val (time1, time2, speedUp) = Bench.compararAlgoritmo(matriz.multMatriz, matriz.mulMatrizPar)(A, B)
      println(s"Tiempo de ejecucion de multMatriz: $time1")
      println(s"Tiempo de ejecucion de mulMatrizPar: $time2")
      println(s"SpeedUp: $speedUp")
    }


  }

  def greeting(): String = "Hello, world!"
}
