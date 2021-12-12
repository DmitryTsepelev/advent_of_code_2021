package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

func getInputData() *[]int {
	file, _ := os.Open("./input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)

	positions := []int{}
	scanner.Scan()
	row := strings.Split(scanner.Text(), ",")

	for _, stringPosition := range row {
		position, _ := strconv.Atoi(stringPosition)
		positions = append(positions, position)
	}

	return &positions
}

func calculateCostConstant(position1 int, position2 int) int {
	return int(math.Abs(float64(position1 - position2)))
}

func calculateCostExponential(position1 int, position2 int) int {
	from := position1
	to := position2
	if from > to {
		from = position2
		to = position1
	}

	cost := 0
	for i := 0; i < to-from; i++ {
		cost += (i + 1)
	}

	return cost
}

func findBestCost(positions []int, exponential bool) int {
	costs := map[int]int{}

	for _, position := range positions {
		cost := 0
		for _, comparedPosition := range positions {
			if exponential {
				cost += calculateCostExponential(comparedPosition, position)
			} else {
				cost += calculateCostConstant(comparedPosition, position)
			}
		}

		costs[position] = cost
	}

	bestPosition := 0
	for position, cost := range costs {
		if cost < costs[bestPosition] {
			bestPosition = position
		}
	}

	return costs[bestPosition]
}

func main() {
	positions := getInputData()
	fmt.Println("Part 1 solution:", findBestCost(*positions, false))
	fmt.Println("Part 2 solution:", findBestCost(*positions, true))
}
