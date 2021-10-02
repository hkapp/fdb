use crate::ghcdump::ir as ghcir;
use crate::ir as cmnir;

pub type ConvError = String;

pub fn conv_decl(ghc_decl: &ghcir::Decl) -> Result<cmnir::Decl, ConvError> {
    Err(String::from("convir::conv_decl() not implemented yet"))
}

pub fn conv_global(ghc_global: ghcir::Global) -> cmnir::Global {
    cmnir::Global(ghc_global.0)
}
