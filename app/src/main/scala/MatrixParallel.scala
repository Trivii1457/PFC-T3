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


    //--------------------------MATRICES-----------------------------------------------------

    //Multiplicaccion estandar secuencial
    def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
        val m2t = transpuesta(m2)
        Vector.tabulate(m1.length, m2t.length){(i,j) => ProductoPunto(m1(i), m2t(j))}
      }

    //Multiplicacion de matrices paralela
    def mulMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
        val m2t = transpuesta(m2)
        val limit = 2

        // inf es el indice inferior de la matriz y sup es el indice superior
        // se toma estos valores para dividir la matriz en dos partes
        def auxPar(inf: Int, sup: Int): Matriz = {
            if(sup - inf <= limit){
                //Aqui se realiza la multiplicacion de matrices, siempre y cuando el tamaño de la matriz sea menor o igual a 4
                val result = Vector.tabulate(m1.length, m2t.length){case (i,j) => ProductoPunto(m1(i), m2t(j))}
                result
            }
            else{
                //Divido la matriz en dos partes
                val mid = inf + (sup - inf) / 2
                val (a, b) = parallel(auxPar(inf, mid), auxPar(mid, sup))//Se realiza la multiplicacion de matrices de manera paralela
                val result = (a ++ b).distinct //Se unen las matrices y se asegura que no haya elementos repetidos
                result//Se retorna la matriz resultante
            }
        }
        auxPar(0, m1.length)
      }

    //Suma de matrices
    def SumMatriz(m1: Matriz, m2: Matriz): Matriz = {
        Vector.tabulate(m1.length, m1.length){(i,j) => m1(i)(j) + m2(i)(j)}
    }

    //Resta de matrices
    def ResMatriz(m1: Matriz, m2: Matriz): Matriz = {
        Vector.tabulate(m1.length, m1.length){(i,j) => m1(i)(j) - m2(i)(j)}
    }


    def MultMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
        val n = m1.length

        if (n == 1) {
            Vector(Vector(m1(0)(0) * m2(0)(0)))
        } else {
            val m = n/2 //Divido la matriz

            //Se extraen las submatrices
            val a11 = SubMatriz(m1, 0, 0, m)
            val a12 = SubMatriz(m1, 0, m, m)
            val a21 = SubMatriz(m1, m, 0, m)
            val a22 = SubMatriz(m1, m, m, m)

            //Se extraen las submatrices
            val b11 = SubMatriz(m2, 0, 0, m)
            val b12 = SubMatriz(m2, 0, m, m)
            val b21 = SubMatriz(m2, m, 0, m)
            val b22 = SubMatriz(m2, m, m, m)

            //Se realiza la multiplicacion de matrices de manera recursiva
            val c11 = SumMatriz(MultMatrizRec(a11, b11), MultMatrizRec(a12, b21))
            val c12 = SumMatriz(MultMatrizRec(a11, b12), MultMatrizRec(a12, b22))
            val c21 = SumMatriz(MultMatrizRec(a21, b11), MultMatrizRec(a22, b21))
            val c22 = SumMatriz(MultMatrizRec(a21, b12), MultMatrizRec(a22, b22))

            c11.zip(c12).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++ c21.zip(c22).map { case (filaC21, filaC22) => filaC21 ++ filaC22}
        }
      }

    //Extraccion de submatriz
    def SubMatriz(m: Matriz, i: Int, j: Int, l: Int ): Matriz = {
        Vector.tabulate(l, l){(x,y) => m(i+x)(j+y)}
    }
  
}
