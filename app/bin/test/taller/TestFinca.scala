package taller
import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TestFinca extends AnyFunSuite{

    type Tablon = (Int, Int, Int)

    type finca = Vector[Tablon]

    type Distancia = Vector[Vector[Int]]

    type ProgRiego = Vector[Int]

    type  TiempoInicioRiego = Vector[Int]

    val Riego = new Finca()
    
  val finca2 = Vector((1, 2, 3), (4, 5, 6))
  val finca3 = Vector((1, 2, 3), (4, 5, 6), (7, 8, 9))
  val finca4 = Vector((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12))
  val finca5 = Vector((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12), (13, 14, 15))
  val finca6 = Vector((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12), (13, 14, 15), (16, 17, 18))

  val progRiego2 = Vector(0, 1)
  val progRiego3 = Vector(0, 1, 2)
  val progRiego4 = Vector(0, 1, 2, 3)
  val progRiego5 = Vector(0, 1, 2, 3, 4)
  val progRiego6 = Vector(0, 1, 2, 3, 4, 5)

  val distancia2: Distancia = Vector(
    Vector(0, 1),
    Vector(1, 0)
  )

  val distancia3: Distancia = Vector(
    Vector(0, 1, 2),
    Vector(1, 0, 1),
    Vector(2, 1, 0)
  )

  val distancia4: Distancia = Vector(
    Vector(0, 1, 2, 3),
    Vector(1, 0, 1, 2),
    Vector(2, 1, 0, 1),
    Vector(3, 2, 1, 0)
  )

  val distancia5: Distancia = Vector(
    Vector(0, 1, 2, 3, 4),
    Vector(1, 0, 1, 2, 3),
    Vector(2, 1, 0, 1, 2),
    Vector(3, 2, 1, 0, 1),
    Vector(4, 3, 2, 1, 0)
  )

  val distancia6: Distancia = Vector(
    Vector(0, 1, 2, 3, 4, 5),
    Vector(1, 0, 1, 2, 3, 4),
    Vector(2, 1, 0, 1, 2, 3),
    Vector(3, 2, 1, 0, 1, 2),
    Vector(4, 3, 2, 1, 0, 1),
    Vector(5, 4, 3, 2, 1, 0)
  )


    /// 5 test para la función costoRiegoTablon
  test("costoRiegoTablon 1") {
    assert(Riego.costoRiegoTablon(0, finca2, progRiego2) == 3)
  }

  test("costoRiegoTablon 2") {
    assert(Riego.costoRiegoTablon(1, finca3, progRiego3) == 18)
  }

  test("costoRiegoTablon 3") {
    assert(Riego.costoRiegoTablon(2, finca4, progRiego4) == 72)
  }

  test("costoRiegoTablon 4") {
    assert(Riego.costoRiegoTablon(3, finca5, progRiego5) == 192)
  }

  test("costoRiegoTablon 5") {
    assert(Riego.costoRiegoTablon(4, finca6, progRiego6) == 405)
  }

  // 5 test para la función costoRiegoFinca
  test("costoRiegoFinca 1") {
    assert(Riego.costoRiegoFinca(finca2, progRiego2) == 21)
  }

  test("costoRiegoFinca 2") {
    assert(Riego.costoRiegoFinca(finca3, progRiego3) == 93)
  }

  test("costoRiegoFinca 3") {
    assert(Riego.costoRiegoFinca(finca4, progRiego4) == 285)
  }

  test("costoRiegoFinca 4") {
    assert(Riego.costoRiegoFinca(finca5, progRiego5) == 690)
  }

  test("costoRiegoFinca 5") {
    assert(Riego.costoRiegoFinca(finca6, progRiego6) == 1428)
  }


  //test costoMovilidad
    test("costoMovilidad 1") {
        assert(Riego.costoMovilidad(finca2, progRiego2, distancia2) == 1)
    }

    test("costoMovilidad 2") {
        assert(Riego.costoMovilidad(finca3, progRiego3, distancia3) == 2)
    }

    test("costoMovilidad 3") {
        assert(Riego.costoMovilidad(finca4, progRiego4, distancia4) == 3)
    }

    test("costoMovilidad 4") {
        assert(Riego.costoMovilidad(finca5, progRiego5, distancia5) == 4)
    }

    test("costoMovilidad 5") {
        assert(Riego.costoMovilidad(finca6, progRiego6, distancia6) == 5)
    }

    //test costoMovilidadPar

    test("costoMovilidadPar 1") {
        assert(Riego.costoMovilidadPar(finca2, progRiego2, distancia2) == 1)
    }

    test("costoMovilidadPar 2") {
        assert(Riego.costoMovilidadPar(finca3, progRiego3, distancia3) == 2)
    }

    test("costoMovilidadPar 3") {
        assert(Riego.costoMovilidadPar(finca4, progRiego4, distancia4) == 3)
    }

    test("costoMovilidadPar 4") {
        assert(Riego.costoMovilidadPar(finca5, progRiego5, distancia5) == 4)
    }

    test("costoMovilidadPar 5") {
        assert(Riego.costoMovilidadPar(finca6, progRiego6, distancia6) == 5)
    }

    //test generarProgramacionesRiego

    test("generarProgramacionesRiego 1") {
        assert(Riego.generarProgramacionesRiego(finca2).length == 2)
    }

    test("generarProgramacionesRiego 2") {
        assert(Riego.generarProgramacionesRiego(finca3).length == 6)
    }

    test("generarProgramacionesRiego 3") {
        assert(Riego.generarProgramacionesRiego(finca4).length == 24)
    }

    test("generarProgramacionesRiego 4") {
        assert(Riego.generarProgramacionesRiego(finca5).length == 120)
    }

    test("generarProgramacionesRiego 5") {
        assert(Riego.generarProgramacionesRiego(finca6).length == 720)
    }

    //test ProgramacionRiegoOptimo

    test("ProgramacionRiegoOptimo 1") {
        assert(Riego.ProgramacionRiegoOptimo(finca2, distancia2)._1.length == 2)
    }

    test("ProgramacionRiegoOptimo 2") {
        assert(Riego.ProgramacionRiegoOptimo(finca3, distancia3)._1.length == 3)
    }

    test("ProgramacionRiegoOptimo 3") {
        assert(Riego.ProgramacionRiegoOptimo(finca4, distancia4)._1.length == 4)
    }

    test("ProgramacionRiegoOptimo 4") {
        assert(Riego.ProgramacionRiegoOptimo(finca5, distancia5)._1.length == 5)
    }

    test("ProgramacionRiegoOptimo 5") {
        assert(Riego.ProgramacionRiegoOptimo(finca6, distancia6)._1.length == 6)
    }

    //test costoRiegoFincaPar

    test("costoRiegoFincaPar 1") {
        assert(Riego.costoRiegoFincaPar(finca2, progRiego2) == 21)
    }

    test("costoRiegoFincaPar 2") {
        assert(Riego.costoRiegoFincaPar(finca3, progRiego3) == 93)
    }

    test("costoRiegoFincaPar 3") {
        assert(Riego.costoRiegoFincaPar(finca4, progRiego4) == 285)
    }

    test("costoRiegoFincaPar 4") {
        assert(Riego.costoRiegoFincaPar(finca5, progRiego5) == 690)
    }

    test("costoRiegoFincaPar 5") {
        assert(Riego.costoRiegoFincaPar(finca6, progRiego6) == 1428)
    }


}
