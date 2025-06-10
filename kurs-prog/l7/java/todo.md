# TODO List

All:
- [ ] Simple textflow and input at the bottom
- [ ] Add some css or styling in the code
- [ ] Make general class (Commands) for parsing the options (commands) provided by the user
  - [ ] 'add' method:
    - [ ] Add a new command to the list with:
      - [ ] a list of names (ex. 'add', 'a')
      - [ ] a description (ex. 'Add a new item')
      - [ ] a callback function to execute when the command is called (the function should accept the command arguments)
      - [ ] a validator
  - [ ] 'parse' method:
    - [ ] Should go through the list of commands and check if all are valid, then invoke the callbacks
    - [ ] If 'help' is called, it should print the list of commands with their descriptions
- [ ] Make TreeManager use the Commands class as well as the ConsoleViewer class
