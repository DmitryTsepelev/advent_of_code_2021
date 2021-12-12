package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func getInputData() []int {
	file, _ := os.Open("./input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)

	depths := []int{}
	for scanner.Scan() {
		depth, _ := strconv.Atoi(scanner.Text())
		depths = append(depths, depth)
	}

	return depths
}

func main() {
	depths := getInputData()

	increases := 0
	prevDepth := 0
	for _, depth := range depths {
		if prevDepth > 0 && depth > prevDepth {
			increases += 1
		}
		prevDepth = depth
	}

	fmt.Println("Part 1 solution:", increases)

	increases = 0
	prevSum := 0
	for i := 0; i < len(depths)-2; i++ {
		currentSum := depths[i] + depths[i+1] + depths[i+2]
		if prevSum > 0 && currentSum > prevSum {
			increases += 1
		}
		prevSum = currentSum
	}

	fmt.Println("Part 2 solution:", increases)
}
