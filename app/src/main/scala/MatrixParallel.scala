package taller
import common.{parallel, task}
import scala.util.Random

class MatrixParallel {

    //Defino el tipo de dato Matriz
    type Matriz = Vector[Vector[Int]]

    //Funciones Auxiliares ---------------------------------------------------
    def MatrizAlAzar(long: Int, vals: Int): Matriz = {
        val V = Vector.fill(long, long) {
            Random.nextInt(vals)
        }
        V
    }

    def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
        val V = Vector.fill(long) {
            Random.nextInt(vals)
        }
        V
    }

    def ProductoPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
        (v1 zip v2).map { case (i, j) => i * j }.sum
    }

    def transpuesta(m: Matriz): Matriz = {
        val l = m.length
        Vector.tabulate(l, l) { (i, j) => m(j)(i) }
    }
    //Fin Funciones Auxiliares ------------------------------------------------ 


    //--------------------------MATRICES-----------------------------------------------------

    //Multiplicaccion estandar secuencial
    def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
        val m2t = transpuesta(m2)
        Vector.tabulate(m1.length, m2t.length) { (i, j) => ProductoPunto(m1(i), m2t(j)) }
    }

    //Multiplicacion de matrices paralela
    def mulMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
        val m2t = transpuesta(m2)
        val limit = 2

        // inf es el indice inferior de la matriz y sup es el indice superior
        // se toma estos valores para dividir la matriz en dos partes
        def auxPar(inf: Int, sup: Int): Matriz = {
            if (sup - inf <= limit) {
                //Aqui se realiza la multiplicacion de matrices, siempre y cuando el tamaño de la matriz sea menor o igual a 4
                val result = Vector.tabulate(m1.length, m2t.length) { case (i, j) => ProductoPunto(m1(i), m2t(j)) }
                result
            }
            else {
                //Divido la matriz en dos partes
                val mid = inf + (sup - inf) / 2
                val (a, b) = parallel(auxPar(inf, mid), auxPar(mid, sup)) //Se realiza la multiplicacion de matrices de manera paralela
                val result = (a ++ b).distinct //Se unen las matrices y se asegura que no haya elementos repetidos
                result //Se retorna la matriz resultante
            }
        }

        auxPar(0, m1.length)
    }

    //Suma de matrices
    def SumMatriz(m1: Matriz, m2: Matriz): Matriz = {
        Vector.tabulate(m1.length, m1.length) { (i, j) => m1(i)(j) + m2(i)(j) }
    }

    //Resta de matrices
    def ResMatriz(m1: Matriz, m2: Matriz): Matriz = {
        Vector.tabulate(m1.length, m1.length) { (i, j) => m1(i)(j) - m2(i)(j) }
    }

    //Multiplicacion de matrices de manera recursiva
    def MultMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
        val n = m1.length

        if (n == 1) {
            Vector(Vector(m1(0)(0) * m2(0)(0)))
        } else {
            val m = n / 2 //Divido la matriz

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

            //Se unen las matrices resultantes, a traves de la funcion zip y map
            c11.zip(c12).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++ c21.zip(c22).map { case (filaC21, filaC22) => filaC21 ++ filaC22 }
        }
    }

    //Extraccion de submatriz
    def SubMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
        Vector.tabulate(l, l) { (x, y) => m(i + x)(j + y) }
    }

    //Multiplicacion de matrices de manera recursiva y paralela
    def MultMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
        val n = m1.length
        if (n == 1) {
            Vector(Vector(m1(0)(0) * m2(0)(0)))
        } else {
            val m = n / 2
            val a11 = SubMatriz(m1, 0, 0, m)
            val a12 = SubMatriz(m1, 0, m, m)
            val a21 = SubMatriz(m1, m, 0, m)
            val a22 = SubMatriz(m1, m, m, m)

            val b11 = SubMatriz(m2, 0, 0, m)
            val b12 = SubMatriz(m2, 0, m, m)
            val b21 = SubMatriz(m2, m, 0, m)
            val b22 = SubMatriz(m2, m, m, m)

            val ((c11, c12), (c21, c22)) = parallel(
                parallel(
                    SumMatriz(MultMatrizRecPar(a11, b11), MultMatrizRecPar(a12, b21)),
                    SumMatriz(MultMatrizRecPar(a11, b12), MultMatrizRecPar(a12, b22))

                ),
                parallel(
                    SumMatriz(MultMatrizRecPar(a21, b11), MultMatrizRecPar(a22, b21)),
                    SumMatriz(MultMatrizRecPar(a21, b12), MultMatrizRecPar(a22, b22))

                )
            )

            c11.zip(c12).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++
              c21.zip(c22).map { case (filaC21, filaC22) => filaC21 ++ filaC22 }

        }


    }

    //Multiplicacion de matrices por strassen
    def MultMatrizStrassen(m1: Matriz, m2: Matriz): Matriz = {
        val n = m1.length
        if (n == 1) {
            Vector(Vector(m1(0)(0) * m2(0)(0)))
        } else {
            val m = n / 2

            val a11 = SubMatriz(m1, 0, 0, m)
            val a12 = SubMatriz(m1, 0, m, m)
            val a21 = SubMatriz(m1, m, 0, m)
            val a22 = SubMatriz(m1, m, m, m)

            val b11 = SubMatriz(m2, 0, 0, m)
            val b12 = SubMatriz(m2, 0, m, m)
            val b21 = SubMatriz(m2, m, 0, m)
            val b22 = SubMatriz(m2, m, m, m)


            val s1 = ResMatriz(b12, b22)
            val s2 = SumMatriz(a11, a12)
            val s3 = SumMatriz(a21, a22)
            val s4 = ResMatriz(b21, b11)
            val s5 = SumMatriz(a11, a22)
            val s6 = SumMatriz(b11, b22)
            val s7 = ResMatriz(a12, a22)
            val s8 = SumMatriz(b21, b22)
            val s9 = ResMatriz(a11, a21)
            val s10 = SumMatriz(b11, b12)

            val p1 = multMatriz(a11, s1)
            val p2 = multMatriz(s2, b22)
            val p3 = multMatriz(s3, b11)
            val p4 = multMatriz(a22, s4)
            val p5 = multMatriz(s5, s6)
            val p6 = multMatriz(s7, s8)
            val p7 = multMatriz(s9, s10)

            val c11 = ResMatriz(SumMatriz(p5,p4), ResMatriz(p2, p6))
            val c12 = SumMatriz(p1, p2)
            val c21 = SumMatriz(p3, p4)
            val c22= ResMatriz(SumMatriz(p5, p1), SumMatriz(p3, p7))

            c11.zip(c12).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++
              c21.zip(c22).map { case (filaC21, filaC22) => filaC21 ++ filaC22 }


        }

    }

    def StrassenParallel(m1: Matriz, m2: Matriz): Matriz = {

        val n = m1.length
        if (n == 1) {
            Vector(Vector(m1(0)(0) * m2(0)(0)))
        } else {
            val m = n / 2

            val a11 = SubMatriz(m1, 0, 0, m)
            val a12 = SubMatriz(m1, 0, m, m)
            val a21 = SubMatriz(m1, m, 0, m)
            val a22 = SubMatriz(m1, m, m, m)

            val b11 = SubMatriz(m2, 0, 0, m)
            val b12 = SubMatriz(m2, 0, m, m)
            val b21 = SubMatriz(m2, m, 0, m)
            val b22 = SubMatriz(m2, m, m, m)


            val s1 = ResMatriz(b12, b22)
            val s2 = SumMatriz(a11, a12)
            val s3 = SumMatriz(a21, a22)
            val s4 = ResMatriz(b21, b11)
            val s5 = SumMatriz(a11, a22)
            val s6 = SumMatriz(b11, b22)
            val s7 = ResMatriz(a12, a22)
            val s8 = SumMatriz(b21, b22)
            val s9 = ResMatriz(a11, a21)
            val s10 = SumMatriz(b11, b12)

            val p1 = multMatriz(a11, s1)
            val p2 = multMatriz(s2, b22)
            val p3 = multMatriz(s3, b11)
            val p4 = multMatriz(a22, s4)
            val p5 = multMatriz(s5, s6)
            val p6 = multMatriz(s7, s8)
            val p7 = multMatriz(s9, s10)

            val ((c11, c12), (c21, c22)) = parallel(
                parallel(
                    ResMatriz(SumMatriz(p5,p4), ResMatriz(p2, p6)),
                    SumMatriz(p1, p2)
                ),
                parallel(

                    SumMatriz(p3, p4),
                    ResMatriz(SumMatriz(p5, p1), SumMatriz(p3, p7))

                )
            )

            c11.zip(c12).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++
              c21.zip(c22).map { case (filaC21, filaC22) => filaC21 ++ filaC22 }


        }

    }
}
