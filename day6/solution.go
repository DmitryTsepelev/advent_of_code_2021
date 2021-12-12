package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func getInputData() *[]int {
	file, _ := os.Open("./input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)

	timers := []int{}
	scanner.Scan()
	row := strings.Split(scanner.Text(), ",")

	for _, stringTimer := range row {
		timer, _ := strconv.Atoi(stringTimer)
		timers = append(timers, timer)
	}

	return &timers
}

func simulate(timers *[]int, dayCount int) int {
	timerMap := map[int]int{}

	for _, timer := range *timers {
		timerMap[timer]++
	}

	for day := 0; day < dayCount; day++ {
		newTimerMap := map[int]int{}

		for timer, count := range timerMap {
			if timer == 0 {
				newTimerMap[6] += count
				newTimerMap[8] += count
			} else {
				newTimerMap[timer-1] += count
			}
		}

		timerMap = newTimerMap
	}

	totalCount := 0
	for _, count := range timerMap {
		totalCount += count
	}

	return totalCount
}

func main() {
	timers := getInputData()
	totalCount := simulate(timers, 80)
	fmt.Println("Part 1 solution:", totalCount)

	timers2 := getInputData()
	totalCount2 := simulate(timers2, 256)
	fmt.Println("Part 2 solution:", totalCount2)
}
