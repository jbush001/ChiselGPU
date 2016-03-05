//
// Copyright 2015 Jeff Bush
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

import Chisel._

object StepDir {
    val step_none :: step_right :: step_down :: step_left :: Nil = Enum(UInt(), 4)
}

//
// Given a triangle, the rasterizer determines which pixels it covers.
// This sweeps over the triangle in a fashion similar to that
// described by Pineda "A parallel algorithm for polygon rasterization"
// (SIGGRAPH 88), Figure 4. It outputs 2x2 aligned quads with one bit per pixel
// to indicate coverage.
//

//
// An edge function determines whether a pixel is on the left or right
// side of a line segment.
//
class EdgeFunction extends Module {
    val io = new Bundle {
        val stepDir = UInt(INPUT, 2)
        val xStep = SInt(INPUT, 32)
        val yStep = SInt(INPUT, 32)
        val setEdgeValue = Bool(INPUT)
        val newEdgeValue = SInt(INPUT, 32)
        val inside = Bool(OUTPUT)
    }

    val edgeValue = Reg(SInt(width = 32))
    when (io.setEdgeValue) {
        edgeValue := io.newEdgeValue
    }
    .elsewhen (io.stepDir === StepDir.step_down) {
        edgeValue := edgeValue + io.yStep
    }
    .elsewhen (io.stepDir === StepDir.step_left) {
        edgeValue := edgeValue - io.xStep
    }
    .elsewhen(io.stepDir === StepDir.step_right) {
        edgeValue := edgeValue + io.xStep
    }

    io.inside := edgeValue(31)
}

//
// Computes three edge functions simulatenously to determine if a pixel
// is inside a triangle.
//
class TriangleFunction extends Module {
    val io = new Bundle {
        val xStep = Vec.fill(3)(SInt(INPUT, 32))
        val yStep = Vec.fill(3)(SInt(INPUT, 32))
        val stepDir = UInt(INPUT, 2)
        val setEdgeValue = Bool(INPUT)
        val newEdgeValue = Vec.fill(3)(SInt(INPUT, 32))
        val inside = Bool(OUTPUT)
    }

    val edges = Vec.tabulate(3)(i => Module(new EdgeFunction).io)
    for (i <- 0 until 3) {
        edges(i).stepDir := io.stepDir
        edges(i).xStep := io.xStep(i)
        edges(i).yStep := io.yStep(i)
        edges(i).setEdgeValue := io.setEdgeValue
        edges(i).newEdgeValue := io.newEdgeValue(i)
    }

    io.inside := edges.map(_.inside).reduceLeft(_ && _)
}

// Determines which pixels of a 2x2 pixel quad are covered by
// a triangle.
// 0 1
// 2 3
class QuadTriangleFunctions extends Module {
    val io = new Bundle {
        val xStep = Vec.fill(3)(SInt(INPUT, 32))
        val yStep = Vec.fill(3)(SInt(INPUT, 32))
        val stepDir = UInt(INPUT, 2)
        val setEdgeValue = Bool(INPUT)
        val newEdgeValue = Vec.fill(3)(SInt(INPUT, 32))
        val coverageMask = UInt(OUTPUT, 4)
    }

    val pixels = Vec.fill(4){ Module(new TriangleFunction()).io }
    for (i <- 0 until 4) {
        pixels(i).xStep := io.xStep
        pixels(i).yStep := io.yStep
        pixels(i).stepDir := io.stepDir
        pixels(i).setEdgeValue := io.setEdgeValue
    }

    io.coverageMask := Cat(pixels.map(_.inside))

    for (i <- 0 until 3) {
        pixels(3).newEdgeValue(i) := io.newEdgeValue(i)
        pixels(2).newEdgeValue(i) := io.newEdgeValue(i) + io.xStep(i)
        pixels(1).newEdgeValue(i) := io.newEdgeValue(i) + io.yStep(i)
        pixels(0).newEdgeValue(i) := io.newEdgeValue(i) + io.xStep(i) +
            io.yStep(i)
    }
}

class Rasterizer(tileSize : Int) extends Module {
    val io = new Bundle {
        val xStep = Vec.fill(3){ SInt(INPUT, 32) }
        val yStep = Vec.fill(3){ SInt(INPUT, 32) }
        val initialXCoord = UInt(INPUT, 16)
        val initialYCoord = UInt(INPUT, 16)
        val initialValue = Vec.fill(3){ SInt(INPUT, 32) }
        val start = Bool(INPUT)
        val busy = Bool(OUTPUT)
        val outputX = UInt(OUTPUT, 16)
        val outputY = UInt(OUTPUT, 16)
        val outputMask = UInt(OUTPUT, 4)
    }

    val xPos = Reg(UInt(width = 16))
    val yPos = Reg(UInt(width = 16))
    val pixelValid = Reg(Bool(), init = Bool(false))
    val stepDir = UInt(width = 2)
    val rasterizationActive = Reg(Bool(), init = Bool(false))

    val quad = Module(new QuadTriangleFunctions)
    quad.io.xStep := io.xStep
    quad.io.yStep := io.yStep
    quad.io.stepDir := stepDir
    quad.io.setEdgeValue := io.start && !rasterizationActive
    quad.io.newEdgeValue := io.initialValue

    io.outputMask := quad.io.coverageMask & Fill(4, pixelValid && rasterizationActive)

    io.outputX := xPos
    io.outputY := yPos
    io.busy := rasterizationActive

    val stateWidth = 3

    def romEntry(stepDir : UInt, pixelValid : Boolean, nextState : Int) : UInt = Cat(stepDir,
        Bool(pixelValid), UInt(nextState, width = stateWidth))

    val notReachable = romEntry(StepDir.step_none, false, 0)
    val rasterizeFinished = romEntry(StepDir.step_none, false, 0)

    // Scan control microcode. Each entry controls which direction to step, whether
    // pixels should be emitted (this is used to avoid emitting pixels twice
    // when searching for an edge), and the next state.
    // There are four transition entries for each state:
    // 0: Outside triangle (coverage mask 0)
    // 1: Inside triangle (coverage mask not 0)
    // 2: On left edge of tile
    // 3: On right edge of tile
    // step_none indicates rasterization is finished
    val stateTransitionTable = Vec(
        // 0: Scan right to find left edge of triangle
        romEntry(StepDir.step_right, false, 0),    // Keep searching right
        romEntry(StepDir.step_right, true, 1),     // Start emitting right
        romEntry(StepDir.step_right, true, 0),    // Start searching right (only happens on start)
        romEntry(StepDir.step_none, false, 0),    // End of triangle

        // 1: Sweep right over inside of triangle
        romEntry(StepDir.step_down, false, 2),     // hit edge, next line
        romEntry(StepDir.step_right, true, 1),     // Inside, keep sweeping
        romEntry(StepDir.step_right, true, 1),  // Left edge of tile, keep scanning
        romEntry(StepDir.step_down, true, 5),     // Skip directly to scanning if we hit edge

        // 2: Stepped down, determine where to go next
        romEntry(StepDir.step_left, false, 3),     // I'm outside, find edge
        romEntry(StepDir.step_right, false, 4), // I'm inside, find edge
        notReachable,
        notReachable,

        // 3: Outside triangle after down. Search left to find right edge
        romEntry(StepDir.step_left, false, 3),  // Keep searching
        romEntry(StepDir.step_left, true, 5),     // Found edge, emit pixel and goto left sweep
        rasterizeFinished,
        notReachable,

        // 4: Inside triangle after down. Scan right to find right edge
        romEntry(StepDir.step_left, false, 5),    // Found edge. Step back into triangle, sweep
        romEntry(StepDir.step_right, false, 4), // keep looking
        notReachable,
        romEntry(StepDir.step_left, true, 5),     // Hit tile edge, emit pixel, keep sweeping

        // 5: Left sweep
        romEntry(StepDir.step_down, false, 6),     // End of sweep
        romEntry(StepDir.step_left, true, 5),    // Keep scanning
        romEntry(StepDir.step_down, true, 1),     // Hit edge, emit pixel, sweep right
        romEntry(StepDir.step_left, true, 5),    // Right edge of tile, keep sweeping

        // 6: Stepped down, determine where to go next
        romEntry(StepDir.step_right, false, 0), // Outside, find edge
        romEntry(StepDir.step_left, false, 7),     // Inside, find edge
        notReachable,
        notReachable,

        // 7: Inside triangle after down. Scan left to find left edge
        romEntry(StepDir.step_left, false, 7),     // Keep searching
        romEntry(StepDir.step_right, true, 1),    // Found triangle edge, step back into triangle
        romEntry(StepDir.step_right, true, 0),    // Hit tile edge, start scanning right
        romEntry(StepDir.step_left, false, 7)    // Stay in this state
    )

    // Scan control microsequencer
    val stateReg = Reg(UInt(width = stateWidth), init = UInt(0))
    val cond = UInt(width = 2)
    when (xPos === UInt(0)) {
        cond := UInt(2)    // Left edge
    }
    .elsewhen (xPos === UInt(tileSize - 1)) {
        cond := UInt(3)    // Right edge
    }
    .elsewhen (quad.io.coverageMask === UInt(0)) {
        cond := UInt(0)    // Outside triangle
    }
    .otherwise {
        cond := UInt(1)    // Inside triangle
    }

    val stateTableEntry = stateTransitionTable(Cat(stateReg, cond))
    stepDir := stateTableEntry(5, 4)
    pixelValid := stateTableEntry(3)

    when (rasterizationActive) {
        when (stepDir === StepDir.step_none || (stepDir === StepDir.step_down
            && yPos === UInt(tileSize - 1))) {
            rasterizationActive := Bool(false)
        }
        .otherwise {
            stateReg := stateTableEntry(2, 0)
        }

        switch (stepDir) {
            is (StepDir.step_left) {
                xPos := xPos - UInt(1)
            }

            is (StepDir.step_down) {
                yPos := yPos + UInt(1)
            }

            is (StepDir.step_right) {
                xPos := xPos + UInt(1)
            }
        }
    }
    .elsewhen (io.start) {
        // Start rasterizing
        stateReg := UInt(0)
        xPos := io.initialXCoord
        yPos := io.initialYCoord
        rasterizationActive := Bool(true)
    }
}
