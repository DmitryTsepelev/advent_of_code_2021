package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

func getInputData() *[][]int {
	file, _ := os.Open("./input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)

	field := [][]int{}
	for scanner.Scan() {
		row := []int{}
		for _, c := range scanner.Text() {
			row = append(row, int(c-'0'))
		}

		field = append(field, row)
	}

	return &field
}

func lowPoint(field [][]int, x int, y int) bool {
	height := field[y][x]

	if x > 0 && field[y][x-1] <= height {
		return false
	}

	if x < len(field[0])-1 && field[y][x+1] <= height {
		return false
	}

	if y > 0 && field[y-1][x] <= height {
		return false
	}

	if y < len(field)-1 && field[y+1][x] <= height {
		return false
	}

	return true
}

func calculateRisk(field [][]int) int {
	risk := 0
	for y := 0; y < len(field); y++ {
		for x := 0; x < len(field[0]); x++ {
			if lowPoint(field, x, y) {
				risk += field[y][x] + 1
			}
		}
	}
	return risk
}

func countBasin(field *[][]int, x int, y int) int {
	if y == -1 || y == len(*field) || x == -1 || x == len((*field)[0]) || (*field)[y][x] == 9 {
		return 0
	}

	(*field)[y][x] = 9

	return 1 +
		countBasin(field, x-1, y) +
		countBasin(field, x+1, y) +
		countBasin(field, x, y-1) +
		countBasin(field, x, y+1)
}

func multiplyBasins(field *[][]int) int {
	basins := []int{}

	for y := 0; y < len(*field); y++ {
		for x := 0; x < len((*field)[0]); x++ {
			basin := countBasin(field, x, y)

			if basin > 0 {
				basins = append(basins, basin)
			}
		}
	}

	sort.Ints(basins)

	result := 1
	for i := len(basins) - 1; i >= len(basins)-3; i-- {
		result *= basins[i]
	}
	return result
}

func main() {
	field := getInputData()

	fmt.Println("Part 1 solution:", calculateRisk(*field))
	fmt.Println("Part 2 solution:", multiplyBasins(field))
}
