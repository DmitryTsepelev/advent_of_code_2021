package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	X int
	Y int
}

type Vent struct {
	Start *Point
	End   *Point
}

func parsePoint(input string) *Point {
	cmp := strings.Split(input, ",")
	x, _ := strconv.Atoi(cmp[0])
	y, _ := strconv.Atoi(cmp[1])
	return &Point{X: x, Y: y}
}

func getInputData() *[]Vent {
	file, _ := os.Open("./input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)

	vents := []Vent{}
	for scanner.Scan() {
		cmp := strings.Split(scanner.Text(), " -> ")

		vent := Vent{Start: parsePoint(cmp[0]), End: parsePoint(cmp[1])}
		vents = append(vents, vent)
	}

	return &vents
}

func buildField(vents *[]Vent) *[][]int {
	maxX := 0
	maxY := 0

	for _, vent := range *vents {
		if vent.Start.X > maxX {
			maxX = vent.Start.X
		}
		if vent.End.X > maxX {
			maxX = vent.End.X
		}
		if vent.Start.Y > maxY {
			maxY = vent.Start.Y
		}
		if vent.End.Y > maxY {
			maxY = vent.End.Y
		}
	}

	field := [][]int{}

	for y := 0; y <= maxY; y++ {
		row := []int{}
		for x := 0; x <= maxX; x++ {
			row = append(row, 0)
		}
		field = append(field, row)
	}

	return &field
}

func printField(field *[][]int) {
	fmt.Println()

	for _, row := range *field {
		for _, value := range row {
			if value > 0 {
				fmt.Print(value)
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

func dangerCellCount(field *[][]int) int {
	count := 0
	for _, row := range *field {
		for _, value := range row {
			if value >= 2 {
				count++
			}
		}
	}

	return count
}

func calculateField(field *[][]int, vents *[]Vent, withDiagonals bool) {
	for _, vent := range *vents {
		startY := vent.Start.Y
		endY := vent.End.Y
		if startY > endY {
			startY = vent.End.Y
			endY = vent.Start.Y
		}

		startX := vent.Start.X
		endX := vent.End.X
		if startX > endX {
			startX = vent.End.X
			endX = vent.Start.X
		}

		if vent.Start.X == vent.End.X || vent.Start.Y == vent.End.Y {
			for x := startX; x <= endX; x++ {
				for y := startY; y <= endY; y++ {
					(*field)[y][x]++
				}
			}
		} else if withDiagonals {
			xCoeff := 0
			yCoeff := 0

			if vent.Start.X < vent.End.X {
				xCoeff = 1
				if vent.Start.Y < vent.End.Y {
					yCoeff = 1
				} else {
					yCoeff = -1
				}
			} else {
				xCoeff = -1
				if vent.Start.Y < vent.End.Y {
					yCoeff = 1
				} else {
					yCoeff = -1
				}
			}

			for offset := 0; offset <= endX-startX; offset++ {
				(*field)[vent.Start.Y+offset*yCoeff][vent.Start.X+offset*xCoeff]++
			}
		}
	}
}

func main() {
	vents := getInputData()

	field := buildField(vents)
	calculateField(field, vents, false)
	fmt.Println("Part 1 solution:", dangerCellCount(field))

	field2 := buildField(vents)
	calculateField(field2, vents, true)
	fmt.Println("Part 2 solution:", dangerCellCount(field2))
}
