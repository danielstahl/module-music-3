package net.soundmining

import java.util.UUID

import net.soundmining.Instrument._
import java.{lang => jl}

/**
  * A development of the Instrument that can  build a graph of
  * instruments and bus arguments are the actual class that generates
  * the bus.
  *
  * val percControl = perc(....)
  * val staticControl = static(440)
  * val triangle = triangle(staticControl, percControl)
  *
  * When you build the triangle you will need the start and
  * end time because you need to know when the buses will will
  * be used.
  *
  * You also need to recursively build all instruments and send them
  * to SC in the correct order. E.g the controllers before the oscillators.
  *
  * Because a controller can be used several times you need to know if
  * a instrument have been generated before. For that reason all instances
  * of a instrument need an id that is unique. UUID.randomUUID().toString?
  *
  * All modular instruments have one output (and one output only). Either
  * to a control bus or a audio bus.
  */
object ModularInstrument {

  trait Bus {
    def busAllocator: BusAllocator
    var busValue: Option[Integer] = None

    def dynamicBus(startTime: Float, endTime: Float, nrOfChannels: Int = 1): Integer = {
      if(busValue.isEmpty) {
        val allocatedBuses = busAllocator.allocate(nrOfChannels, startTime, endTime)
        this.busValue = Option(allocatedBuses.head)
      }
      busValue.get
    }

    def staticBus(busValue: Integer): Integer = {
      this.busValue = Option(busValue)
      this.busValue.get
    }
  }

  class AudioBus extends Bus {
    def busAllocator: BusAllocator = BusAllocator.audio

  }

  class ControlBus extends Bus {
    def busAllocator: BusAllocator = BusAllocator.control
  }

  trait ModularInstrument {
    type SelfType <: ModularInstrument
    type OutputBusType <: Bus
    type OutputInstrumentType <: ModularInstrument

    def self(): SelfType

    val id: String = UUID.randomUUID().toString
    val instrumentName: String

    var addAction: AddAction = HEAD_ACTION

    def addAction(value: AddAction): SelfType = {
      addAction = value
      self()
    }

    var nodeId: Node = SOURCE

    def nodeId(value: Node): SelfType = {
      nodeId = value
      self()
    }

    var optionalDur: Option[Float] = None

    def withDur(value: Float): SelfType = {
      optionalDur = Option(value)
      self()
    }

    var nrOfChannels: Int = 1

    def withNrOfChannels(value: Int): SelfType = {
      nrOfChannels = value
      self()
    }

    /**
      * return this instruments output bus
      */
    def getOutputBus: OutputBusType

    def withOutput(output: OutputInstrumentType): SelfType

    /**
      * Build a flattened graph of this instrument and it's children
      */
    def graph(parent: Seq[ModularInstrument]): Seq[ModularInstrument]

    /**
      * Prepend this instrument builder to the flattened graph. Makes sure that it gets
      * built as early as possible
      */
    def prependToGraph(parent: Seq[ModularInstrument]): Seq[ModularInstrument] =
      if (parent.exists(instrument => instrument.id == this.id)) {
        parent
      } else {
        this +: parent
      }

    /**
      * Append this instrument builder to the flattened graph. Also makes sure
      * that its not added if it's already there.
      */
    def appendToGraph(parent: Seq[ModularInstrument]): Seq[ModularInstrument] =
      if (parent.exists(instrument => instrument.id == this.id)) {
        parent
      } else {
        parent :+ this
      }

    /**
      * Builds a flattened graph of instrument builders
      */
    def buildGraph(startTime: Float, duration: Float, flatGraph: Seq[ModularInstrument]): Seq[Seq[Object]] =
      flatGraph.map(instrument => instrument.build(startTime, duration))
        .filter(graph => graph.nonEmpty)

    def internalBuild(startTime: Float, duration: Float): Seq[Object]

    var instrumentIsBuilt = false
    /**
      * Builds this instrument
      */
    def build(startTime: Float, duration: Float): Seq[Object] = {
      if(!instrumentIsBuilt) {
        val finalDuration = buildFloat(optionalDur.getOrElse(duration))

        val graph = Seq(
          instrumentName,
          Integer.valueOf(-1), addAction.action, nodeId.nodeId,
          "out", buildInteger(getOutputBus.dynamicBus(startTime, startTime + finalDuration, nrOfChannels)),
          "dur", finalDuration
        ) ++ internalBuild(startTime, duration)
        instrumentIsBuilt = true
        graph
      } else {
        Seq()
      }

    }
  }

  trait AudioInstrument extends ModularInstrument {
    override type OutputBusType = AudioBus
    override type OutputInstrumentType = AudioInstrument
    var bus = new AudioBus
    override def getOutputBus: AudioBus = bus

    def withOutput(output: AudioInstrument): SelfType = {
      this.bus = output.getOutputBus
      self()
    }
  }

  trait ControlInstrument extends ModularInstrument {
    override type OutputBusType = ControlBus
    override type OutputInstrumentType = ControlInstrument

    var bus = new ControlBus
    override def getOutputBus: ControlBus = bus

    override def withOutput(output: ControlInstrument): SelfType = {
      this.bus = output.getOutputBus
      self()
    }
  }

  class StaticAudioBusInstrument extends AudioInstrument {
    override type SelfType = StaticAudioBusInstrument

    override def self(): SelfType = this

    override val instrumentName: String = "NONE"

    override def graph(parent: Seq[ModularInstrument]): Seq[ModularInstrument] = parent

    override def internalBuild(startTime: Float, duration: Float): Seq[Object] = Seq.empty
  }
}
