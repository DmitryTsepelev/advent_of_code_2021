package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func getInputData() (*[]int, *[][][]*int) {
	file, _ := os.Open("./input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)

	numbers := []int{}
	scanner.Scan()
	for _, stringNum := range strings.Split(scanner.Text(), ",") {
		number, _ := strconv.Atoi(stringNum)
		numbers = append(numbers, number)
	}

	boards := [][][]*int{}
	var currentBoard [][]*int
	idx := 0

	for scanner.Scan() {
		stringNumbers := strings.Fields(scanner.Text())

		if len(stringNumbers) == 0 {
			currentBoard = make([][]*int, 5)
			idx = 0
			boards = append(boards, currentBoard)
		} else {
			row := []*int{}
			for _, stringNum := range stringNumbers {
				number, _ := strconv.Atoi(stringNum)
				row = append(row, &number)
			}

			currentBoard[idx] = row
			idx++
		}
	}

	return &numbers, &boards
}

func boardIsWinner(board [][]*int) bool {
	for _, line := range board {
		crossed := true

		for _, number := range line {
			if number != nil {
				crossed = false
			}
		}

		if crossed {
			return true
		}
	}

	for col := 0; col < 5; col++ {
		crossed := true

		for _, line := range board {
			if line[col] != nil {
				crossed = false
			}
		}

		if crossed {
			return true
		}
	}

	return false
}

func boardSum(board [][]*int) int {
	sum := 0

	for _, line := range board {
		for _, number := range line {
			if number != nil {
				sum += *number
			}
		}
	}

	return sum
}

func part1Solution(numbers []int, boards [][][]*int) int {
	for _, currentNumber := range numbers {
		for _, board := range boards {
			for _, line := range board {
				for index, lineNumber := range line {
					if lineNumber != nil && *lineNumber == currentNumber {
						line[index] = nil
					}
				}
			}

			if boardIsWinner(board) {
				return boardSum(board) * currentNumber
			}
		}
	}

	return 0
}

func part2Solution(numbers []int, boards [][][]*int) int {
	for _, currentNumber := range numbers {
		newBoards := [][][]*int{}
		winnerCountBefore := len(boards)

		for _, board := range boards {
			for _, line := range board {
				for index, lineNumber := range line {
					if lineNumber != nil && *lineNumber == currentNumber {
						line[index] = nil
					}
				}
			}

			if winnerCountBefore == 1 && boardIsWinner(board) {
				return boardSum(board) * currentNumber
			} else if !boardIsWinner(board) {
				newBoards = append(newBoards, board)
			}
		}

		boards = newBoards
	}

	return 0
}

func main() {
	numbers, boards := getInputData()
	fmt.Println("Part 1 solution:", part1Solution(*numbers, *boards))
	fmt.Println("Part 2 solution:", part2Solution(*numbers, *boards))
}
