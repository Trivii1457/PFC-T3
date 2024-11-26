package taller
import common.{parallel, task}
import scala.util.Random

class MatrixParallel {

    //Defino el tipo de dato Matriz
    type Matriz =  Vector[Vector[Int]]

    //Funciones Auxiliares ---------------------------------------------------
    def MatrizAlAzar(long : Int, vals : Int): Matriz = {
        val V = Vector.fill(long, long){Random.nextInt(vals)}
        V
    }

    def vectorAlAzar(long : Int, vals : Int): Vector[Int] = {
        val V = Vector.fill(long){Random.nextInt(vals)}
        V
    }

    def ProductoPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
        (v1 zip v2).map{case (i,j) => i*j}.sum
    }

    def transpuesta(m: Matriz): Matriz = {
        val l = m.length
        Vector.tabulate(l, l){(i,j) => m(j)(i)}
    }
    //Fin Funciones Auxiliares ------------------------------------------------ 

    //Multiplicaccion estandar secuencial
    def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
        val m2t = transpuesta(m2)
        Vector.tabulate(m1.length, m2t.length){(i,j) => ProductoPunto(m1(i), m2t(j))}
      }

    //Extraccion de submatriz
    def SubMatriz(m: Matriz, i: Int, j: Int, l: Int ): Matriz = {
        Vector.tabulate(l, l){(x,y) => m(i+x)(j+y)}
    }
  
}
