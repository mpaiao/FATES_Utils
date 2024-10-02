#==========================================================================================
#   TRY utilities for standardising number of replicates and value kind name.
#
#   Author: Marcos Longo 
#   Email:  m l o n g o a@t l b l d.o.t g o v
#
#   Date: 10-Oct-2023
#
#   Suite of functions to fix columns "Replicates" and "ValueKindName", and to discard
# data if needed.
#------------------------------------------------------------------------------------------



#---~---
#   This function fixes the value kind name and assigns it to a few defined categories, 
# and decide what to do with the number of replicates (force them to 1 or keep the
# original value) and the trait values (keep them or discard them depending on the 
# category). The following input variables are needed.
# 
# - RawTRY       -- The tibble with the raw TRY data (i.e., the one when we read the 
#                   input TRY csv files).
# - discard_rm   -- Remove rows flagged as discard?
# - duplicate_rm -- Remove rows flagged as duplicates?
# - ValueKind    -- Column that contains the value kind names. Default is the name used
#                   in the TRY data base.
# - Count        -- Column that countains the data count. Default is the name used in the
#                   TRY data base.
#---~---
TRY_Fix_ValueKindName <<- function( RawTRY
                                  , discard_rm   = TRUE
                                  , duplicate_rm = TRUE
                                  , ValueKind    = "ValueKindName"
                                  , Count        = "Replicates"
                                  , OrigObsID    = "OrigObsDataID"
                                  ){

   #---~---
   #   Create some derived variables and define the output tibble
   #---~---
   if ("Author" %in% names(RawTRY)){
      AuthorName          = RawTRY$Author
   }else{
      AuthorName          = paste(RawTRY$FirstName,RawTRY$LastName)
   }#end if ("Author" %in% names(RawTRY))
   OutTRY              = RawTRY
   OutTRY[[ValueKind]] = tolower(OutTRY[[ValueKind]])
   #---~---


   #---~---
   #   Test the variable types.
   #---~---
   IsUnknown = is.na(OutTRY[[ValueKind]])
   IsSingle  = OutTRY[[ValueKind]] %in% c( "best estimate", "single")
   IsAverage = 
      OutTRY[[ValueKind]] %in% c( "average", "mean", "site specific mean", "species mean"
                                , "class mean"
                                )#end c
   IsMedian  = OutTRY[[ValueKind]] %in% "median"
   IsDiscard = 
      OutTRY[[ValueKind]] %in% c( "high", "low", "lower quartile", "maximum"
                                , "minimum", "upper quartile", "upper 95 percentile"
                                )#end c
   IsTrouble = ! ( IsUnknown | IsSingle | IsAverage | IsMedian | IsDiscard )
   #---~---


   #---~---
   #   If we find any value kind name not previously identified, we issue an error message
   # and stop the function.
   #---~---
   if (any(IsTrouble)){
      InvalidKind = sort(unique(OutTRY[[ValueKind]][IsTrouble]))
      cat0(" ")
      cat0("--------------")
      cat0(" FATAL ERROR! ")
      cat0("--------------")
      cat0(" Author: ",paste(sort(unique(AuthorName)),collapse="   "),".")
      cat0(" The following variable kind names were found, but they are not recognised.")
      for (i in seq_along(InvalidKind)) cat0(" - \"",InvalidKind[i],"\".")
      cat0("--------------")
      stop("Error loading trait data!")
   }#end if (any(IsTrouble))
   #---~---


   #---~---
   #   Update column `ValueKindName` to the default categories.
   #---~---
   OutTRY[[ValueKind]][IsSingle ] = "Single"
   OutTRY[[ValueKind]][IsAverage] = "Average"
   OutTRY[[ValueKind]][IsMedian ] = "Median"
   OutTRY[[ValueKind]][IsUnknown] = "Unknown"
   OutTRY[[ValueKind]][IsDiscard] = "Discard"
   #---~---

   #---~---
   #   Order value kind name, which will be handy for comparisons when merging multiple
   # traits to the same observation.s
   #---~---
   LevelsKindName      = c("Discard","Unknown","Median","Average","Single")
   OutTRY[[ValueKind]] = ordered(x=OutTRY[[ValueKind]],levels=LevelsKindName)
   #---~---


   #---~---
   #   Ensure that the number of counts does not exceed 1 when row is flagged as single
   # measurement, and ensure count is numeric
   #---~---
   OutTRY[[Count]]           = suppressWarnings(as.numeric(OutTRY[[Count]]))
   OutTRY[[Count]][IsSingle] = pmin(OutTRY[[Count]][IsSingle],1.,na.rm=TRUE)
   #---~---

   #---~---
   #   If wanted, suppress lines where that contain values we are not seeking.
   #---~---
   OutTRY      = OutTRY[! ( IsDiscard & discard_rm ),,drop=FALSE]
   #---~---


   #---~---
   #   We also duplicate observations that have been identified as duplicates.
   #---~---
   if (OrigObsID %in% names(OutTRY)){
      IsDuplicate = ! is.na(OutTRY[[OrigObsID]])
      OutTRY      = OutTRY[! ( IsDuplicate & duplicate_rm ),,drop=FALSE]
   }#end if (OrigObsID %in% names(OutTRY))
   #---~---

   #---~---
   #   Return standardised tibble.
   #---~---
   return(OutTRY)
   #---~---

}#end function TRY_Fix_ValueKindName
#---~---
