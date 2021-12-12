package main

import (
	"bufio"
	"fmt"
	"os"
)

func getInputData() *[][]byte {
	file, _ := os.Open("./input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)

	numbers := [][]byte{}
	for scanner.Scan() {
		value := []byte(scanner.Text())
		numbers = append(numbers, value)
	}

	return &numbers
}

func getInt(s []byte) int {
	var res int
	for _, v := range s {
		res <<= 1
		res |= int(v)
	}
	return res
}

func getMostCommonValue(index int, numbers *[][]byte, controlByte byte) bool {
	count := 0

	for _, row := range *numbers {
		if row[index] == controlByte {
			count += 1
		} else {
			count -= 1
		}
	}

	return count >= 0
}

func getRate(numbers *[][]byte, controlByte byte) int {
	rate := []byte{}

	for i := 0; i < len((*numbers)[0]); i++ {
		mostCommon := getMostCommonValue(i, numbers, controlByte)

		if mostCommon {
			rate = append(rate, 1)
		} else {
			rate = append(rate, 0)
		}
	}

	intRate := getInt(rate)

	return intRate
}

func main() {
	numbers := *getInputData()

	gammaRate := getRate(&numbers, 49)
	epsilonRate := getRate(&numbers, 48)
	fmt.Println("Part 1 solution:", gammaRate*epsilonRate)

	filteredNumbers := []byte{}
	for {
		if len(numbers) == 1 {
			break
		}
	}
}
