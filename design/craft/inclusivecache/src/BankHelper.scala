package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._

object BankHelper {
    def bankIndexHelper(address: UInt, mask: UInt): UInt = {
      if ( mask != 0 ) {
        //Get bit positions in mask
        def bankBits = (0 until mask.getWidth).filter( i => (mask.litValue & ( 1L << i )) != 0 )
        //Index address with those bit positions
        def bank = bankBits.map(address(_))
        //Convert Bool seq to Vec to UInt
        VecInit(bank).asUInt
      } else {
        0.U
      }
    }

}