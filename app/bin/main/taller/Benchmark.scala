package taller
import org.scalameter._

class Benchmark {

    type Tablon = (Int, Int, Int)

    type finca = Vector[Tablon]

    type Distancia = Vector[Vector[Int]]

    type ProgRiego = Vector[Int]

    type  TiempoInicioRiego = Vector[Int]

    val Riego = new Finca()

    //--------------------------------------------------------------------------------


    def comparacionCostoRiego(f: finca, p: ProgRiego): List[Double] = {
        
        val timeSeq = withWarmer(new Warmer.Default) measure {
            Riego.costoRiegoFinca(f, p)
        }

        val timePar = withWarmer(new Warmer.Default) measure {
            Riego.costoRiegoFincaPar(f, p)
        }

        val ts = timeSeq.value
        val tp = timePar.value
        List(ts, tp, ts/tp)
        
    }

    def comparacionCostoMov(f: finca, p: ProgRiego, d: Distancia): List[Double] = {
        
        val timeSeq = withWarmer(new Warmer.Default) measure {
            Riego.costoMovilidad(f, p, d)
        }

        val timePar = withWarmer(new Warmer.Default) measure {
            Riego.costoMovilidadPar(f, p, d)
        }

        val ts = timeSeq.value
        val tp = timePar.value
        List(ts, tp, ts/tp)
        
    }

    def compararProgramaciones(f: finca, d: Distancia): List[Double] = {
        
        val timeSeq = withWarmer(new Warmer.Default) measure {
            Riego.ProgramacionRiegoOptimo(f, d)
        }

        val timePar = withWarmer(new Warmer.Default) measure {
            Riego.ProgramacionRiegoOptimoPar(f, d)
        }

        val ts = timeSeq.value
        val tp = timePar.value
        List(ts, tp, ts/tp)
        
    }

    def compararProgOptimas(f: finca, d: Distancia): List[Double] = {
        
        val timeSeq = withWarmer(new Warmer.Default) measure {
            Riego.ProgramacionRiegoOptimo(f, d)
        }

        val timePar = withWarmer(new Warmer.Default) measure {
            Riego.ProgramacionRiegoOptimoPar(f, d)
        }

        val ts = timeSeq.value
        val tp = timePar.value
        List(ts, tp, ts/tp)
        
    }


  
}