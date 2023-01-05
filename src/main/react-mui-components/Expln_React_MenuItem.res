open Expln_React_common

@module("@mui/material/MenuItem") @react.component
external make: (
    ~value:string,
    ~children:reElem=?
) => reElem = "default"