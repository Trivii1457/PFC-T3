package taller
import org.scalameter._

class Benchmark {

    val Riego = new Finca()

    type Tablon = (Int, Int, Int)

    type Finca = Vector[Tablon]

    type Distancia = Vector[Vector[Int]]

    type ProgRiego = Vector[Int]

    type  TiempoInicioRiego = Vector[Int]

    //--------------------------------------------------------------------------------


    def comparacionCostoRiego(f: Finca, p: ProgRiego): List[Double] = {
        
        val timeSeq = whitWarmer(new Warmer.Default) measure {
            Riego.costoRiegoFinca(f, p)
        }

        val timePar = whitWarmer(new Warmer.Default) measure {
            Riego.costoRiegoFincaPar(f, p)
        }

        List(timeSeq, timePar, timeSeq/timePar)
        
    }

    def comparacionCostoMov(f: Finca, p: ProgRiego, d: Distancia): List[Double] = {
        
        val timeSeq = whitWarmer(new Warmer.Default) measure {
            Riego.costoMovilidad(f, p, d)
        }

        val timePar = whitWarmer(new Warmer.Default) measure {
            Riego.costoMovilidadPar(f, p, d)
        }

        List(timeSeq, timePar, timeSeq/timePar)
        
    }

    def compararProgramaciones(f: Finca, d: Distancia): List[Double] = {
        
        val timeSeq = whitWarmer(new Warmer.Default) measure {
            Riego.ProgramacionRiegoOptimo(f, d)
        }

        val timePar = whitWarmer(new Warmer.Default) measure {
            Riego.ProgramacionRiegoOptimoPar(f, d)
        }

        List(timeSeq, timePar, timeSeq/timePar)
        
    }

    def compararProgOptimas(f: Finca, d: Distancia): List[Double] = {
        
        val timeSeq = whitWarmer(new Warmer.Default) measure {
            Riego.ProgramacionRiegoOptimo(f, d)
        }

        val timePar = whitWarmer(new Warmer.Default) measure {
            Riego.ProgramacionRiegoOptimoPar(f, d)
        }

        List(timeSeq, timePar, timeSeq/timePar)
        
    }


  
}