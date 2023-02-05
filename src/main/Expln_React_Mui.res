
module Accordion = Expln_React_Accordion
module AccordionDetails = Expln_React_AccordionDetails
module AccordionSummary = Expln_React_AccordionSummary
module Box = Expln_React_Box
module Button = Expln_React_Button
module Checkbox = Expln_React_Checkbox
module Col = Expln_React_Column
module Dialog = Expln_React_Dialog
module Divider = Expln_React_Divider
module FormControl = Expln_React_FormControl
module FormControlLabel = Expln_React_FormControlLabel
module Grid = Expln_React_Grid
module IconButton = Expln_React_IconButton
module Input = Expln_React_Input
module InputAdornment = Expln_React_InputAdornment
module InputLabel = Expln_React_InputLabel
module List = Expln_React_List
module ListItem = Expln_React_ListItem
module ListItemButton = Expln_React_ListItemButton
module ListItemIcon = Expln_React_ListItemIcon
module ListItemText = Expln_React_ListItemText
module MenuItem = Expln_React_MenuItem
module Pagination = Expln_React_Pagination
module Paper = Expln_React_Paper
module Row = Expln_React_Row
module Select = Expln_React_Select
module Slider = Expln_React_Slider
module Tab = Expln_React_Tab
module Tabs = Expln_React_Tabs
module TextField = Expln_React_TextField

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
