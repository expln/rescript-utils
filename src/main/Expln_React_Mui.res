
module TextField = Expln_React_TextField
module Grid = Expln_React_Grid
module Col = Expln_React_Column
module Row = Expln_React_Row
module Paper = Expln_React_Paper
module Button = Expln_React_Button
module Checkbox = Expln_React_Checkbox
module FormControl = Expln_React_FormControl
module InputLabel = Expln_React_InputLabel
module Select = Expln_React_Select
module MenuItem = Expln_React_MenuItem
module InputAdornment = Expln_React_InputAdornment
module IconButton = Expln_React_IconButton
module List = Expln_React_List
module ListItem = Expln_React_ListItem
module ListItemButton = Expln_React_ListItemButton
module ListItemText = Expln_React_ListItemText
module ListItemIcon = Expln_React_ListItemIcon
module Slider = Expln_React_Slider
module Input = Expln_React_Input
module Box = Expln_React_Box

module TextFileReader = Expln_React_TextFileReader

module Icons = {

  module Delete = {
    @module("@mui/icons-material/Delete") @react.component
    external make: () => React.element = "default"
  }

  module Clear = {
    @module("@mui/icons-material/Clear") @react.component
    external make: () => React.element = "default"
  }

  module BrightnessLow = {
    @module("@mui/icons-material/BrightnessLow") @react.component
    external make: () => React.element = "default"
  }
}
