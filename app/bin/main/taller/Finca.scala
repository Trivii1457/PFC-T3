package taller
import commom.Parallel
import java.util.Random
import  scala.collection.parallel.CollectionConverters._


class Finca {
  


    //Definicion de los tipos de datos

    type Tablon = (Int, Int, Int)

    type Finca = Vector[Tablon]

    type Distancia = Vector[Vector[Int]]

    type ProgRiego = Vector[Int]

    type  TiempoInicioRiego = Vector[Int]

    //--------------------------------------------------------------------------------

    //Randomizador

    val random = new Random()

    //Funciones propuestas por el profesor

    def FincaALAzar(long: Int): Finca = {

        val vec = Vector.fill(long)(

            (random.nextInt(long*2)+1, random.nextInt(long*2)+1, random.nextInt(4)+1)
        )
        vec
    }

    def DistanciaAlAzar(long: Int): Distancia = {

        val vec = Vector.fill(long, long)(

            random.nextInt(long*3) + 1  
        )
        Vector.tabulate(long, long)((i, j) => 
            if(i < j) vec(j)(i)
            else if(i == j) 0 
            else vec(i)(j))
    }


    //exploracion de entradas

    def tsup(f: Finca, i: Int): Int = {f(i)._1}

    def treg(f: Finca, i: Int): Int = {f(i)._2}

    def prio(f: Finca, i: Int): Int = {f(i)._3}

    //calculo de tiempo de inicio de riego

    def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {


        val tiempos = Array.fill(f.length)(0)

        for (j <- until to pi.length) {

            val prevTablon = pi(j -  1)
            val currentTablon = pi(j)
            tiempos(currentTablon) =  tiempos(prevTablon) + treg(f, prevTablon)

        }

        tiempos.toVector
    }


    //Calculando costos
    def costoRiegoTablon(i:Int, f:Finca, pi:ProgRiego) : Int = {
        val tiempoInicio = tIR(f, pi)(i)
        val tiempoFinal = tiempoInicio + treg(f, i)
        if (tsup(f,i) - treg(f, i) >= tiempoInicio) {
            tsup(f,i) - tiempoFinal
        } else  {
            prio(f,i) * (tiempoFinal - tsup(f,i))
        }
    }

    def costoRiegoFinca(f:Finca, pi:ProgRiego) : Int = {
        (0 until f.length).map(i => costoRiegoTablon(i, f, pi)).sum
    }

    def costoMovilidad(f:Finca, pi:ProgRiego, d:Distancia) : Int = {
        (0 until pi.length - 1).map(j => d(pi(j))(pi(j+1))).sum
    }

    def generarProgramacionesRiego(f:Finca) : Vector[ProgRiego] = {
        val indices = (0 until f.length).toVector
        indices.permutations.toVector
    }

    def ProgramacionRiegoOptimo(f:Finca, d:Distancia) : (ProgRiego, Int) = {
        val programaciones = generarProgramacionesRiego(f)
        val costos = programaciones.map(pi => 
            (pi, costoRiegoFinca(f, pi) + costoMovilidad(f, pi, d))
        )
        costos.minBy(_._2) 
    }

    //Versiones paralelas

    def costoRiegoFincaPar(f:Finca, pi:ProgRiego) : Int = {
        (0 until f.length).par.map(i => costoRiegoTablon(i, f, pi)).sum
    }

    def costoMovilidadPar(f:Finca,pi:ProgRiego, d:Distancia) : Int = {
        (0 until pi.length - 1).par.map(j => d(pi(j))(pi(j+1))).sum
    }

    def generarProgramacionesRiegoPar(f:Finca) : Vector[ProgRiego] = {
        val indices = (0 until f.length).toVector
        indices.permutations.toVector.par.toVector
    }

    def ProgramacionRiegoOptimoPar(f:Finca, d:Distancia) : (ProgRiego, Int) = {
        val programaciones = generarProgramacionesRiegoPar(f)
        val costos = programaciones.par.map(pi => 
            (pi, costoRiegoFincaPar(f, pi) + costoMovilidadPar(f, pi, d))
        )
        costos.minBy(_._2)
    }
}
