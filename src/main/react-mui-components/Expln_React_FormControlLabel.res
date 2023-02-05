open Expln_React_common

@module("@mui/material/FormControlLabel") @react.component
external make: (
    ~control: reElem, 
    ~label: string, 
) => reElem = "default"
