package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Command struct {
	Kind   string
	Length int
}

func getInputData() *[]*Command {
	file, _ := os.Open("./input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)

	commands := []*Command{}
	for scanner.Scan() {
		cmp := strings.Split(scanner.Text(), " ")
		length, _ := strconv.Atoi(cmp[1])

		command := Command{Kind: cmp[0], Length: length}
		commands = append(commands, &command)
	}

	return &commands
}

func main() {
	commands := *getInputData()

	position := 0
	depth := 0
	for _, command := range commands {
		switch command.Kind {
		case "forward":
			position += command.Length
		case "up":
			depth -= command.Length
		case "down":
			depth += command.Length
		}
	}

	fmt.Println("Part 1 solution:", position*depth)

	position = 0
	depth = 0
	aim := 0
	for _, command := range commands {
		switch command.Kind {
		case "forward":
			position += command.Length
			depth += aim * command.Length
		case "up":
			aim -= command.Length
		case "down":
			aim += command.Length
		}
	}

	fmt.Println("Part 2 solution:", position*depth)
}
