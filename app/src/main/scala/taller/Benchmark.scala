package taller

import org.scalameter._

class Benchmark {

  type Matriz = Vector[Vector[Int]]
  type Algoritmo = (Matriz, Matriz) => Matriz

  def compararAlgoritmo(a1: Algoritmo, a2: Algoritmo)(m1: Matriz, m2: Matriz): (Double, Double, Double) = {

    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure {a1(m1,m2)}

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure {a2(m1,m2)}

    val speedUp= timeA1.value - timeA2.value


    (timeA1.value, timeA2.value , speedUp)
  }

  def CompararProductoPunto(a1: () => Int, a2: () => Int): (Double, Double, Double) = {

    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure {a1()}

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure {a2()}

    val speedUp = timeA1.value - timeA2.value

    (timeA1.value, timeA2.value, speedUp)
  }
}