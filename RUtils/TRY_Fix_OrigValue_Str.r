#==========================================================================================
#   TRY utilities for loading traits and ancillary data
#
#   Author: Marcos Longo 
#   Email:  m l o n g o a@t l b l d.o.t g o v
#
#   Date: 25-Feb-2022
#
#   Suite of functions to fix some of the TRY data base entries
#------------------------------------------------------------------------------------------



#---~---
#   This function fixes traits when loading the original string value.  The following
# input variables are needed.
#
# - TraitID     -- ID given for this trait by the TRY data base. This must be a scalar.
# - Type        -- Variable type ("numeric","integer","character","logical"). This must
#                  be a scalar too.
# - TraitOrig   -- Original string values for this trait. Either a scalar or a vector
# - UnitOrig    -- Original string with units for this trait. Either a scalar or a vector,
#                  dimension must match TraitOrig.
# - NameOrig    -- Original name of trait. This is useful for separating multiple variables
#                  shared by the same TraitID.
# - AuthorName  -- Author name. This is useful for separating multiple variables shared by 
#                  the same TraitID.
# - UniqOutput  -- Write unique values to this file. If NULL, data are not written.
# - OutputName  -- Names for output variables.  For most cases, this is completely 
#                  irrelevant and should only have a single name. The only exceptions are:
#                  - Leaf strength. Currently this must be a vector of four elements 
#                    corresponding to either FT (leaf tensile strength, kN/m), FP (leaf 
#                    resistance to puncture, kN/m), TT (leaf tensile toughness, kN/m2) and
#                    TP (leaf puncturability toughness, kN/m2). If the vector of length 
#                    four does not have names, the code will assume the names are in the
#                    order above. Otherwise, provide the vector with named elements
#                    (example: OutputName = c( FT="leaf_f_tear", FP="leaf_f_punct",...)
#                    If OutputName=NULL, the code will assign the default values.
#                  - Xylem vulnerability potential: currently this must be a vector
#                    of five elements corresponding to the potential at 12, 20, 50, 80, 
#                    and 88% loss of conductivity.  If the vector of length five doesn't 
#                    have names, the code will assume the names are in the 12-88 order. 
#                    Otherwise, the vector elements must be named P12, P20, ..., P88.
#                    (example: OutputName = c(P12="xylem_p12",P20="xylem_p20",...))
#                    If OutputName = NULL, the code will assign the default values.
#---~---
TRY_FixTrait_OrigValue_Str <<- function( TraitID,Type,TraitOrig,UnitOrig,NameOrig
                                       , AuthorName, UniqOutput=NULL, OutputName = NULL){

   #---~---
   #   Make sure dimensions of input values make sense.
   #---~---
   CntTraitID   = length(TraitID  )
   CntType      = length(Type     )
   CntTraitOrig = length(TraitOrig)
   CntUnitOrig  = length(UnitOrig )
   if ( (CntTraitID != 1L) || (CntType != 1L) || (CntTraitOrig != CntUnitOrig) ){
      cat0(" ")
      cat0("------------------")
      cat0("   FATAL ERROR!   ")
      cat0("------------------")
      cat0(" - Length of variable \"TraitID\":    ",CntTraitID  ,".")
      cat0(" - Length of variable \"Type\":       ",CntType     ,".")
      cat0(" - Length of variable \"TraitOrig\":  ",CntTraitOrig,".")
      cat0(" - Length of variable \"UnitOrig\":   ",CntUnitOrig ,".")
      cat0(" ")
      cat0(" Variables \"TraitID\" and \"Type\" must be scalars.")
      cat0(" Dimensions of variables  \"TraitOrig\" and \"UnitOrig\" must match.")
      cat0("------------------")
      stop(" Inconsistent settings.")
   }#end if ( (CntTraitID != 1L) || (CntType != 1L) || (CntTraitOrig != CntUnitOrig) )
   #---~---


   #---~---
   #   Assign list of output variable names in case it's not provided.
   #---~---
   if (is.null(OutputName)){
      #---~---
      #   Give the standard names for leaf toughness and xylem vulnerability (or nothing 
      # otherwise).
      #---~---
      if (TraitID %in% c(2L)){
         OutputName = c( FT = "leaf_f_tear"
                       , FP = "leaf_f_punct"
                       , TT = "leaf_t_tear"
                       , TP = "leaf_t_punct"
                       )#end c
      }else if (TraitID %in% c(719L)){
         OutputName = c( P12 = "xylem_p12"
                       , P20 = "xylem_p20"
                       , P50 = "xylem_p50"
                       , P80 = "xylem_p80"
                       , P88 = "xylem_p88"
                       )#end c
      }else{
         OutputName = paste0("trait_",sprintf("%4.4i",TraitID))
      }#end if
      #---~---
   }else if (TraitID %in% c(2L)){
      CntOutputName = length(OutputName)
      #---~---

      #---~---
      #   User provided names, make sure the length is correct and name the names.
      #---~---
      if (CntOutputName %in% 4L){
         if (is.null(names(OutputName))){
            names(OutputName) = c("FT","FP","TT","TP")
         }else if (! all(c("FT","FP","TT","TP") %in% names(OutputName))){
            cat0(" ")
            cat0("------------------")
            cat0("   FATAL ERROR!   ")
            cat0("------------------")
            cat0(" - Vector element names for variable \"OutputName\" must be.")
            cat0("      \"FT\", \"FP\", \"TT\", and \"TP\".")
            cat0("   when  when TraitID = 2L (Leaf texture).")
            cat0(" - Alternatively, provide the vector without names by setting:")
            cat0("      names(OutputName) = NULL    before calling the function.")
            cat0("------------------")
            stop(" Inconsistent settings.")
         }#end if
      }else{
         cat0(" ")
         cat0("------------------")
         cat0("   FATAL ERROR!   ")
         cat0("------------------")
         cat0(" - Length of variable \"OutputName\": ",CntOutputName  ,".")
         cat0(" ")
         cat0(" - \"OutputName\" must be a vector of length 4 when TraitID = 2L")
         cat0("   (Leaf texture).")
         cat0("------------------")
         stop(" Inconsistent settings.")
      }#end if
   }else if (TraitID %in% c(719L)){
      CntOutputName = length(OutputName)
      #---~---

      #---~---
      #   User provided names, make sure the length is correct and name the names.
      #---~---
      if (CntOutputName %in% 5L){
         if (is.null(names(OutputName))){
            names(OutputName) = c("P12","P20","P50","P80","P88")
         }else if (! all(c("P12","P20","P50","P80","P88") %in% names(OutputName))){
            cat0(" ")
            cat0("------------------")
            cat0("   FATAL ERROR!   ")
            cat0("------------------")
            cat0(" - Vector element names for variable \"OutputName\" must be.")
            cat0("      \"P12\", \"P20\", \"P50\", \"P80\", and \"P88\".")
            cat0("   when  when TraitID = 719L (Xylem hydraulic vulnerability).")
            cat0(" - Alternatively, provide the vector without names by setting:")
            cat0("      names(OutputName) = NULL    before calling the function.")
            cat0("------------------")
            stop(" Inconsistent settings.")
         }#end if
      }else{
         cat0(" ")
         cat0("------------------")
         cat0("   FATAL ERROR!   ")
         cat0("------------------")
         cat0(" - Length of variable \"OutputName\": ",CntOutputName  ,".")
         cat0(" ")
         cat0(" - \"OutputName\" must be a vector of length 5 when TraitID = 719L")
         cat0("   (Xylem hydraulic vulnerability).")
         cat0("------------------")
         stop(" Inconsistent settings.")
      }#end if
      #---~---
   }#end if
   #---~---



   #---~---
   #   Initialise vector with Values and Validity. We flag traits that should become
   # invalid due to known limitations in the input data.
   #---~---
   Value  = TraitOrig
   Valid  = ! is.na(Value)
   #---~---


   #---~---
   #   Output name: assign the variable names to the output (except leaf texture and 
   # xylem vulnerability).
   #---~---
   if (TraitID %in% c(2L,719L,3479L)){
      VName  = rep(x=NA_character_,times=length(Value))
   }else{
      VName  = ifelse(test=Valid,yes=OutputName,no=NA_character_)
   }#end if (TraitID %in% c(719L,3479L))
   #---~---



   #---~---
   #   Flag for writing table of unique values.
   #---~---
   WriteUnique = ! is.null(UniqOutput)
   #---~---

   #---~---
   #   Some entries have odd characters.
   #---~---
   Value = gsub(pattern="\u0092",replacement="'",x=Value)
   Value = gsub(pattern="\u0096",replacement="-",x=Value)
   Value = gsub(pattern="\u00a0",replacement="" ,x=Value)
   Value = gsub(pattern="<92>"  ,replacement="'",x=Value)
   Value = gsub(pattern="<96>"  ,replacement="-",x=Value)
   Value = gsub(pattern="<a0>"  ,replacement="" ,x=Value)
   #---~---

   #---~---
   #   Make original name lower case
   #---~---
   NameOrig = tolower(NameOrig)
   #---~---


   #--- Initial changes to the trait, to remove spurious characters.
   if (Type %in% c("numeric","integer")){
      #---~---
      #   Fix specific oddities here, as there are no generalisable rules for some of
      # them...
      #---~---
      Value[Value %in% "(0.2-)0.9-2(-3) m"] = "0.2-3 m"
      Value[Value %in% "(0.6)1.5(2) m"    ] = "0.6-2 m"
      Value[Value %in% "0.16.0.6 m"       ] = "0.16-0.6 m"
      Value[Value %in% "10-(40)m"         ] = "10-40 m"
      Value[Value %in% "12- (15) m"       ] = "12-15 m"
      Value[Value %in% "13-(25) m"        ] = "13-25 m"
      Value[Value %in% "15 -(25 m)"       ] = "15-25 m"
      Value[Value %in% "15-(25) m"        ] = "15-25 m"
      Value[Value %in% "2.;5-10 m"        ] = "2-10  m"
      Value[Value %in% "20- (40) m"       ] = "20-40 m"
      Value[Value %in% "20-(30) m"        ] = "20-30 m"
      Value[Value %in% "20-100 vm"        ] = "20-100 cm"
      Value[Value %in% "24-(45) m"        ] = "24-45 m"
      Value[Value %in% "4-(9) m"          ] = "4-9 m"
      Value[Value %in% "50-20 (30) cm"    ] = "20-50 cm"
      Value[Value %in% "to 40 m30-40 m"   ] = "30-40 m"
      Value[Value %in% "1; 2; more"       ] = ">1"
      Value[Value %in% "11 and 3"         ] = "3-11"
      Value[Value %in% "14 and 2"         ] = "2-14"
      Value[Value %in% "2 and ?"          ] = ">2"
      Value[Value %in% "2 and 3"          ] = "2-3"
      Value[Value %in% "2 and?"           ] = ">2"
      Value[Value %in% "3 and xx"         ] = ">3"
      Value[Value %in% "4 and ?"          ] = ">4"
      Value[Value %in% "5 and 5"          ] = "5"
      Value[Value %in% "6 and ?"          ] = ">6"
      Value[Value %in% "6 and xx"         ] = ">6"
      #---~---


      #---~---
      #   Some entries have lots of extra information.
      #---~---
      tmi_sel        = grepl(pattern="^a mean of",x=Value)
      Value[tmi_sel] = gsub(pattern=";"               ,replacement=".",x=Value[tmi_sel])
      Value[tmi_sel] = gsub(pattern="^a mean of"      ,replacement="" ,x=Value[tmi_sel])
      Value[tmi_sel] = gsub(pattern="at 50 cm height$",replacement="" ,x=Value[tmi_sel])
      #---~---


      #---~---
      #   Plant longevity has a mix of numeric and text. Some of the text is useful to 
      # define life span, so we turn them into strings that can be turned into numeric
      # by the general cases below.
      #---~---
      if (TraitID %in% c(59L)){


         #---~---
         #   Some authors provided plant age instead of plant longevity.
         #---~---
         IsAuthor = ( AuthorName %in% "Fritz Schweingruber" ) & ( NameOrig %in% "age" )
         Value[IsAuthor] = NA_character_
         Valid[IsAuthor] = FALSE
         VName[IsAuthor] = NA_character_
         #---~---



         #---~---
         #   Replace units and values for those with accurate description. Others will
         # be discarded unfortunately.
         #---~---
         IsAnnual    = tolower(Value) %in% c( "always annual", "ann", "annual", "annual"
                                            , "annual-winter annual", "annual/ephemeral"
                                            , "annuals", "summer annuals", "winter annual"
                                            , "winter annuals"
                                            )#end c
         IsAnnBien   = tolower(Value) %in% c( "always annual; sometimes biennial"
                                            , "annual-biennial"
                                            , "annual-winter annual; biennial"
                                            , "annual; biennial", "annual/bieenial"
                                            , "annual/biennial", "annual/biennual"
                                            , "annual/bisannual"
                                            , "sometimes annual; always biennial"
                                            , "winter annual-biennial"
                                            )#end c
         IsBiennial  = tolower(Value) %in% c( "always biennial", "bennial", "biannual"
                                            , "biasannual", "biennial", "biennial"
                                            )#end c
         #---- Update values.
         Value   [IsAnnual  ] = "1"
         Value   [IsAnnBien ] = "1-2"
         Value   [IsBiennial] = "2"
         UnitOrig[IsAnnual  ] = "year"
         UnitOrig[IsAnnBien ] = "year"
         UnitOrig[IsBiennial] = "year"
         #---~---


         #---~---
         #   Use brute force for a few of then
         #---~---
         Is1050           = tolower(Value) %in% "from few decades to more than 50 years"
         Value   [Is1050] = "20-59"
         UnitOrig[Is1050] = "year"
         Is1060           = tolower(Value) %in% "from few decades to more than 60 years"
         Value   [Is1060] = "20-69"
         UnitOrig[Is1060] = "year"
         IsGt50           = tolower(Value) %in% "long lived perennial >= 50 yrs."
         Value   [IsGt50] = ">50"
         UnitOrig[IsGt50] = "year"
         IsLt20           = tolower(Value) %in% "perennial < 20 years"
         Value   [IsLt20] = "<20"
         UnitOrig[IsLt20] = "year"
         IsLt50           = tolower(Value) %in% "perennial <50 yrs"
         Value   [IsLt50] = "<50"
         UnitOrig[IsLt50] = "year"
         IsGt20           = tolower(Value) %in% "perennial > 20 years"
         Value   [IsGt20] = ">20"
         UnitOrig[IsGt20] = "year"
         IsLt05           = tolower(Value) %in% "poly-annuals < 5 years (short-lived perennials)"
         Value   [IsLt05] = "<5"
         UnitOrig[IsLt05] = "year"
         IsGt50           = tolower(Value) %in% "poly-annuals > 50 years (long-lived perennials)"
         Value   [IsGt50] = ">50"
         UnitOrig[IsGt50] = "year"
         Is0550           = tolower(Value) %in% "poly-annuals 5-50 years (medium-lived perennials)"
         Value   [Is0550] = "5-50"
         UnitOrig[Is0550] = "year"
      }#end if (TraitID %in% 59)
      #---~---







      #---~---
      #   A basic requirement for numeric and integer traits is to have numbers...
      #---~---
      bad_sel        = ! grepl(pattern="[0-9]",x=Value)
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---



      #---~---
      #   Some data sets are too cryptic to be fixed. Set them to NA.
      #---~---
      bad_sel        = Value %in% c("to 60 am","<1 few flowers to 1.5")
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---



      #---~---
      #   Number values. Make sure any comma or semi-colon becomes a dot
      # and remove spurious spaces, which could make otherwise valid values 
      # become NA.
      #---~---
      Value = gsub(pattern=",",replacement=".",x=Value)
      Value = gsub(pattern=";",replacement=".",x=Value)
      Value = gsub(pattern=" ",replacement="" ,x=Value)
      #---~---


      #---~---
      #   Remove spurious characters, including uncertainties
      #---~---
      Value = gsub(pattern=" "          ,replacement="" ,x=Value)
      Value = gsub(pattern=";"          ,replacement=".",x=Value)
      Value = gsub(pattern="\\(.*\\)"   ,replacement="" ,x=Value)
      Value = gsub(pattern="\\-\\(.*\\)",replacement="" ,x=Value)
      Value = gsub(pattern="\\+\\/\\-.*",replacement="" ,x=Value)
      Value = gsub(pattern="\\+\\-.*"   ,replacement="" ,x=Value)
      #---~---





      #---~---
      #   Inventory all types of data available from this data set
      #---~---
      #--- For simplicity, we only keep classes with letters.
      if (WriteUnique & any(Valid)){
         #--- Check whether to write a new file or continue previous.
         NewFile  = (! file.exists(UniqOutput) )
         #---~---



         #---~---
         #   Create a tibble with the data
         #---~---
         UniqName = tibble( AuthorName = AuthorName[Valid]
                          , NameOrig   = tolower(NameOrig[Valid])
                          , UnitOrig   = ifelse( test = is.na(UnitOrig[Valid])
                                               , yes  = "dimensionless"
                                               , no   = UnitOrig[Valid]
                                               )#end ifelse
                          )#end tibble
         #---~---

         #---~---
         #   Aggregate categorical data
         #---~---
         sort_uniq = function(x) sort(unique(x))
         nl_table  = function(x) tabulate(factor(x=x,levels=sort_uniq(x)))
         UniqName = 
            UniqName                                                 %>%
            group_by(NameOrig)                                       %>%
            reframe( AuthorName = commonest(AuthorName,na.rm=TRUE)
                   , CntUnit    = nl_table (UnitOrig)
                   , UnitOrig   = sort_uniq(UnitOrig)              ) %>%
            select(c("AuthorName","NameOrig","UnitOrig","CntUnit"))
         #---~---



         #---~---
         #   Write file
         #---~---
         dummy = write_delim( x         = UniqName
                            , file      = UniqOutput
                            , append    = ! NewFile
                            , col_names = NewFile
                            , quote     = "needed"
                            , delim     = ","
                            )#end write_delim
         #---~---
      }#end if (WriteUnique)
      #---~---


   }else if (Type %in% c("character")){
      #---~---
      # Character values. There shouldn't be any commas, but just in case, we
      # update them. Also, if there is any empty string, with replace them 
      # with NA_character_
      #---~---
      Value          = gsub(pattern=",",replacement=";",x=Value)
      bad_sel        = Value %in% c("","na","not available")
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---





      #---~---
      #   Inventory all types of data available from this data set
      #---~---
      #--- For simplicity, we only keep classes with letters.
      UseValue = Valid & grepl(pattern="[a-z]",x=tolower(Valid))
      if (WriteUnique & any(UseValue)){
         #--- Check whether to write a new file or continue previous.
         NewFile  = (! file.exists(UniqOutput) )
         #---~---



         #---~---
         #   Create a tibble with the data
         #---~---
         UniqName = tibble( AuthorName = AuthorName[UseValue]
                          , NameOrig   = tolower(NameOrig[UseValue])
                          , Value      = tolower(Value   [UseValue])
                          )#end tibble
         #---~---

         #---~---
         #   Aggregate categorical data
         #---~---
         sort_uniq = function(x) sort(unique(x))
         nl_table  = function(x) tabulate(factor(x=x,levels=sort_uniq(x)))
         UniqName = 
            UniqName                                                 %>%
            group_by(NameOrig)                                       %>%
            reframe( AuthorName = commonest(AuthorName,na.rm=TRUE)
                   , CntValue   = nl_table (Value)
                   , Value      = sort_uniq(Value)                 ) %>%
            select(c("AuthorName","NameOrig","Value","CntValue"))
         #---~---



         #---~---
         #   Write file
         #---~---
         dummy = write_delim( x         = UniqName
                            , file      = UniqOutput
                            , append    = ! NewFile
                            , col_names = NewFile
                            , quote     = "needed"
                            , delim     = ","
                            )#end write_delim
         #---~---
      }#end if (WriteUnique)
      #---~---



   }else{
      # Other types. Not really sure what to do other than replacing
      # commas with semi-colons.
      Value = gsub(pattern=",",replacement=";",x=Value)
   }#end if (Type %in% c("numeric","integer"))
   #---~---


   #---~---
   #    The following block is a long list of cases specific for each trait data.
   # This block may need revision if more trait data are needed and as more data are
   # contributed to TRY data base.
   #---~---
   if (TraitID %in% c(2L)){
      #---~---
      #   2 - Leaf texture (sclerophylly; physical strength; toughness)
      #---~---



      #---~---
      #   Initialise flags for output name to be all FALSE. Also initialise the puncture
      # circumference (in m) and tear cross-section width (also in m), which may be needed
      # if authors provide force in Newtons. These values depend on the instrument used and
      # may vary by author.
      #---~---
      IsFT   = rep(FALSE   ,times=length(NameOrig))
      IsFP   = rep(FALSE   ,times=length(NameOrig))
      IsTT   = rep(FALSE   ,times=length(NameOrig))
      IsTP   = rep(FALSE   ,times=length(NameOrig))
      CPunct = rep(NA_real_,times=length(NameOrig))
      WTear  = rep(NA_real_,times=length(NameOrig))
      #---~---



      #---~---
      #   Some authors provided sclerophylly classification. Delete them.
      #---~---
      #--- Fatih Fazlioglu
      IsTMI        =
         ( AuthorName %in% "Fatih Fazlioglu" ) & ( NameOrig   %in% "leaf condition" )
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Walton Green
      IsTMI        =
         ( AuthorName %in% "Walton Green" ) & ( NameOrig   %in% "foliage texture" )
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Sandy Harrison
      IsTMI        =
         ( ( AuthorName %in% "Sandy Harrison" )
         & ( NameOrig   %in% c( "leaf texture: coriaceous", "leaf texture: fleshy"
                              , "leaf texture: leathery", "leaf texture: malacophyll"
                              , "leaf texture: papery", "leaf texture: rigidly coriaceous"
                              )#end c
           )#end NameOrig
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Ingolf Kuhn
      IsTMI        =
         ( AuthorName %in% "Ingolf Kuhn" ) & ( NameOrig   %in% "leafmorphology" )
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Michelle Leishman
      IsTMI        =
         ( AuthorName %in% "Michelle Leishman" ) & ( NameOrig   %in% "lt; leaf texture" )
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Renske Onstein
      IsTMI        =
         ( ( AuthorName %in% "Renske Onstein" )
         & ( NameOrig   %in% "leaf_sclerophylly_thickness_description" )
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Han Wang
      IsTMI        =
         ( AuthorName %in% "Han Wang" ) & ( NameOrig   %in% "leaf texture" )
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---



      #---~---
      #   Some data seem to be bogus entries.
      #---~---
      Discard        =
         ( ( AuthorName %in% "Justin Wright"               )
         & ( NameOrig   %in% "treatment water table depth" )
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Define data based on author, original units, and name.
      #---~---
      #--- Peter Adler, name and units indicate leaf tensile strength.
      IsAuthor = 
         ( AuthorName %in% "Peter Adler" ) & (NameOrig %in% "leaf tensile strength" )
      IsTT = IsTT | IsAuthor
      #---~---
      #   Chris Baraloto, force to puncture provided. However, no diameter of the puncture
      # element was provided. The supporting information indicates that they used a
      # Chatillon E-DFE-2 push gauge (penetrometer). Upon reaching out to Chatillon, they
      # informed me that the factory puncture element diameter is 4.75 mm.
      #---~---
      IsAuthor = 
         ( AuthorName %in% "Chris Baraloto" ) & (NameOrig %in% "toughness" )
      IsFP             = IsFP | IsAuthor
      CPunct[IsAuthor] = pi * 0.00475
      #--- Vanessa Boukili, assume FP based on the reference paper.
      IsAuthor = 
         ( AuthorName %in% "Vanessa Boukili" ) & (NameOrig %in% "leaf toughness" )
      IsFP     = IsFP | IsAuthor
      #--- Marc Cadotte, assume FP based on the reference paper.
      IsAuthor = 
         ( AuthorName %in% "Marc Cadotte" ) & (NameOrig %in% "toughness (g/mm)" )
      IsFP     = IsFP | IsAuthor
      #--- Johannes Cornelissen, tensile strength provided.
      IsAuthor = 
         ( ( AuthorName %in% "Johannes Cornelissen" ) 
         & (NameOrig %in% c("leaf toughness (tensile strength)","tensile strength (n/mm)") )
         )#end IsAuthor
      IsFT     = IsFT | IsAuthor
      #--- Sandra Diaz, the comments indicate they measured tensile strength
      IsAuthor = 
         ( AuthorName %in% "Sandra Diaz" ) & (NameOrig %in% "leaf toughness (nmm-1)" )
      IsFT     = IsFT | IsAuthor
      #--- Bryan Finegan, the original name indicates leaf tensile strength
      IsAuthor = 
         ( AuthorName %in% "Bryan Finegan" ) & (NameOrig %in% "lts" )
      IsFT     = IsFT | IsAuthor
      #---~---
      #   Pengcheng He, they provided both tear and puncture data. They also provided
      # values normalised by LMA, but we delete these.
      #---~---
      IsAuthor     = AuthorName %in% "Pengcheng He"
      IsTMI        = IsAuthor & ( NameOrig %in% c("fp/lma","ft/ lma") )
      IsFT         = IsFT | ( IsAuthor & ( NameOrig %in% c("ft") ) )
      IsFP         = IsFP | ( IsAuthor & ( NameOrig %in% c("fp") ) )
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---
      #   Herve Jactel, leaf toughness provided, but unclear whether this is tear- or
      # puncture-tested. The data are also provided in Newtons and no publication is 
      # associated with this data. For the time being we discard the data.
      #---~---
      IsAuthor = 
         ( AuthorName %in% "Herve Jactel" ) & (NameOrig %in% "leaf.toughness" )
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #--- Kim La Pierre, puncture pressure according to meta data
      IsAuthor = 
         ( AuthorName %in% "Kim La Pierre" ) & (NameOrig %in% "leaf_toughness" )
      IsTP     = IsTP | IsAuthor
      #---~---
      #   Tara Massad. They provide puncture pressure according to meta data. Their
      # units are rather confusing (they added the punch area in the denominator), but
      # based on the distribution of values, it seems they meant g/mm2, after they divided
      # by the punch area.
      #---~---
      IsAuthor = 
         ( AuthorName %in% "Tara Massad" ) & (NameOrig %in% "mean_leaf_toughness" )
      IsTP     = IsTP | IsAuthor
      #--- Rebecca Montgomery, leaf tensile strength
      IsAuthor = 
         ( AuthorName %in% "Rebecca Montgomery" ) & (NameOrig %in% "leaf tensile strength")
      IsFT     = IsFT | IsAuthor
      #---~---
      #   Yusuke Onoda. They provide tensile and puncture strength and pressure. They also
      # provide work to shear, but we don't keep these for now.
      #---~---
      IsAuthor     = AuthorName %in% "Yusuke Onoda"
      IsTMI        = IsAuthor & ( NameOrig %in% c("ws","wss") )
      IsFT         = IsFT | ( IsAuthor & ( NameOrig %in% c("ft" ) ) )
      IsTT         = IsTT | ( IsAuthor & ( NameOrig %in% c("fts") ) )
      IsFP         = IsFP | ( IsAuthor & ( NameOrig %in% c("fp" ) ) )
      IsTP         = IsTP | ( IsAuthor & ( NameOrig %in% c("fps") ) )
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Valerio Pillar, leaf tensile specific strength
      IsAuthor = 
         ( ( AuthorName %in% "Valerio Pillar" )
         & (NameOrig %in% "leaf tensile strength average (n/mm2)")
         )#end IsAuthor
      IsTT     = IsTT | IsAuthor
      #--- Lourens Poorter, leaf specific puncturability
      IsAuthor = 
         ( AuthorName %in% "Lourens Poorter" ) & (NameOrig %in% "lto")
      IsTP     = IsTP | IsAuthor
      #--- Arthur Vinicius Rodrigues, leaf puncture strength (based on reference).
      IsAuthor = 
         ( ( AuthorName %in% "Arthur Vinicius Rodrigues" )
         & (NameOrig %in% "leaftough_n/mm")
         )#end IsAuthor
      IsFP     = IsFP | IsAuthor
      #---~---
      #   Marko Spasojevic, force to puncture provided. However, no diameter of the
      # puncture element was provided.  For now we discard the data.
      #---~---
      IsAuthor = 
         ( AuthorName %in% "Marko Spasojevic" ) & (NameOrig %in% "leaftoughness" )
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---
      #   Tanya Strydom, force to puncture provided. However, no diameter of the puncture
      # element was provided. For now we discard the data.
      #---~---
      IsAuthor = 
         ( AuthorName %in% "Tanya Strydom" ) & (NameOrig %in% "toughness" )
      # IsTP     = IsTP | IsAuthor
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---
      #   Stephni van der Merwe, force to puncture provided. However, no diameter of the
      # puncture element was provided. For now we discard the data.
      #---~---
      IsAuthor = 
         ( AuthorName %in% "Stephni van der Merwe" ) & (NameOrig %in% "penetrometer (n)" )
      #IsTP     = IsTP | IsAuthor
      Value     [IsAuthor] = NA_character_
      Valid     [IsAuthor] = FALSE
      VName     [IsAuthor] = NA_character_
      #--- Masha van der Sande, leaf puncture pressure (based on reference).
      IsAuthor = 
         ( ( AuthorName %in% "Masha van der Sande"     )
         & ( NameOrig   %in% "specific force to punch" )
         )#end IsAuthor
      IsTP     = IsTP | IsAuthor
      #--- Jaime Villacis, leaf tensile strength (based on original name).
      IsAuthor = 
         ( ( AuthorName %in% "JAIME VILLACIS"     )
         & ( NameOrig   %in% "fuerza tensil foliar  (n/mm)" )
         )#end IsAuthor
      IsFT     = IsFT | IsAuthor
      #---~---
      #   Alexandra Weigelt, force to punch provided. According to the supporting 
      # information of their reference paper (https://doi.org/10.1111/1365-2745.12489), 
      # the needle diameter was 1.4 mm.
      #---~---
      IsAuthor         =
         ( ( AuthorName %in% "Alexandra Weigelt" )
         & ( NameOrig   %in% "leaftoughness"     )
         )#end IsAuthor
      IsFP             = IsFP | IsAuthor
      CPunct[IsAuthor] = pi * 0.0014
      #---~---


      #--- Make sure all data have the same units (N/mm).
      kgT_sel          =  IsFT          & ( UnitOrig %in% c("kg"       ) )
      kgP_sel          =  IsFP          & ( UnitOrig %in% c("kg"       ) )
      NT_sel           =  IsFT          & ( UnitOrig %in% c("N"        ) )
      NP_sel           =  IsFP          & ( UnitOrig %in% c("N"        ) )
      gomm_sel         = (IsFT | IsFP ) & ( UnitOrig %in% c("g/mm"     ) )
      Nocm_sel         = (IsFT | IsFP ) & ( UnitOrig %in% c("(N/10)/mm") )
      kNom_sel         = 
         ( (IsFT | IsFP )
         & ( UnitOrig %in% c("N/mm","Newton_mm-1","kN m-1","Newtons/mm","N/mm#") )
         )#end knom_sel
      MNom2_sel        = 
         ( (IsTT | IsTP )
         & ( UnitOrig %in% c("N/mm2","Newtons mm-2","MN m-2","n/mm2") )
         )#end mnom2_sel
      Nocm2_sel        = (IsTT | IsTP ) & ( UnitOrig %in% c("N/cm2") )
      gomm2_sel        = (IsTT | IsTP ) & ( UnitOrig %in% c("g/mm2","g/(15.85mm2)") )
      Value[NT_sel   ] = as.character(        N.2.kN / WTear  * as.numeric(Value[NT_sel   ]) )
      Value[NP_sel   ] = as.character(        N.2.kN / CPunct * as.numeric(Value[NP_sel   ]) )
      Value[kgT_sel  ] = as.character( grav * N.2.kN / WTear  * as.numeric(Value[kgT_sel  ]) )
      Value[kgP_sel  ] = as.character( grav * N.2.kN / CPunct * as.numeric(Value[kgP_sel  ]) )
      Value[gomm_sel ] = as.character(            g2kg * grav * as.numeric(Value[gomm_sel ]) )
      Value[Nocm_sel ] = as.character(                    10. * as.numeric(Value[Nocm_sel ]) )
      Value[kNom_sel ] = as.character(                          as.numeric(Value[kNom_sel ]) )
      Value[MNom2_sel] = as.character(                          as.numeric(Value[MNom2_sel]) )
      Value[Nocm2_sel] = as.character(      N.2.MN * m2.2.cm2 * as.numeric(Value[Nocm2_sel]) )
      Value[gomm2_sel] = as.character(            g2kg * grav * as.numeric(Value[gomm2_sel]) )
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = 
         Valid & ( NT_sel    | NP_sel    | kgT_sel   | kgP_sel   | Nocm_sel  | gomm_sel  
                 | kNom_sel  | MNom2_sel | Nocm2_sel | gomm2_sel )
      Value[! IsValid] = NA_character_
      #---~---


      #---~---
      #   Assign output names.
      #---~---
      VName[IsFT] = OutputName["FT"]
      VName[IsFP] = OutputName["FP"]
      VName[IsTT] = OutputName["TT"]
      VName[IsTP] = OutputName["TP"]
      #---~---


   }else if (TraitID %in% c(6L,18L,21L,46L,773L,1777L,2545L,3106L,3107L)){
      #---~---
      #   6    - Root rooting depth
      #   18   - Plant height
      #   21   - Stem diameter
      #   46   - Leaf thickness
      #   773  - Crown (canopy) height (base to top)
      #   1777 - Fine root rooting depth
      #   2545 - Belowground plant organ rooting depth
      #   3106 - Plant height vegetative
      #   3107 - Plant height generative
      #---~---



      #---~---
      #   Some authors provided multiple estimates for the same quantity, delete all but
      # one.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Colleen Iversen" )
                 & ( NameOrig %in% c( "rooting depth_extrapolated 50 percent rooting depth"
                                    , "rooting depth_interpolated 50 percent rooting depth"
                                    , "rooting depth_interpolated 95 percent rooting depth"
                                    , "rooting depth_max"
                                    , "rooting depth_min"
                                    )#end c
                   )#end NameOrig
                 )#end IsAuthor
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---



      #---~---
      #   Discard bad values or values with odd units.
      #---~---
      bad_unit       = UnitOrig %in% c("%","text")
      bad_value      = Value    %in% c("simple","10/01/1930","herb","shrub")
      bad_sel        = bad_unit | bad_value
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---



      #--- Remove spurious characters (but track those with cm in the value).
      has_um = ( grepl(pattern="mm$"     ,x=Value)
               | grepl(pattern="mm.$"    ,x=Value) )
      has_mm = ( grepl(pattern="mm$"     ,x=Value)
               | grepl(pattern="mm.$"    ,x=Value) )
      has_in = ( grepl(pattern="in$"     ,x=Value)
               | grepl(pattern="in.$"    ,x=Value)
               | grepl(pattern="inch$"   ,x=Value)
               | grepl(pattern="inches$" ,x=Value) )
      has_cm = ( grepl(pattern="cm$"     ,x=Value)
               | grepl(pattern="cm.$"    ,x=Value)
               | grepl(pattern="cn$"     ,x=Value)
               | grepl(pattern="cn.$"    ,x=Value)
               | grepl(pattern="c$"      ,x=Value)
               | grepl(pattern="vm$"     ,x=Value) )
      has_dm = ( grepl(pattern="dm$"     ,x=Value)
               | grepl(pattern="dm.$"    ,x=Value) )
      has_ft = ( grepl(pattern="\\'$"    ,x=Value)
               | grepl(pattern="\\`$"    ,x=Value)
               | grepl(pattern="ft$"     ,x=Value)
               | grepl(pattern="ft.$"    ,x=Value)
               | grepl(pattern="feet$"   ,x=Value)
               | grepl(pattern="feet.$"  ,x=Value) )
      has_m  = ( grepl(pattern="[0-9]m$" ,x=Value) 
               | grepl(pattern="[0-9]n$" ,x=Value) 
               | grepl(pattern="[0-9]m.$",x=Value) )
      Value  = gsub(pattern="mm$"     ,replacement="",x=Value)
      Value  = gsub(pattern="mm.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="mm$"     ,replacement="",x=Value)
      Value  = gsub(pattern="mm.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="in$"     ,replacement="",x=Value)
      Value  = gsub(pattern="in.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="inch$"   ,replacement="",x=Value)
      Value  = gsub(pattern="inches$" ,replacement="",x=Value)
      Value  = gsub(pattern="cn$"     ,replacement="",x=Value)
      Value  = gsub(pattern="cn.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="cm$"     ,replacement="",x=Value)
      Value  = gsub(pattern="cm.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="c$"      ,replacement="",x=Value)
      Value  = gsub(pattern="dm$"     ,replacement="",x=Value)
      Value  = gsub(pattern="dm.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="\\'$"    ,replacement="",x=Value)
      Value  = gsub(pattern="\\`$"    ,replacement="",x=Value)
      Value  = gsub(pattern="ft$"     ,replacement="",x=Value)
      Value  = gsub(pattern="ft.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="feet$"   ,replacement="",x=Value)
      Value  = gsub(pattern="feet.$"  ,replacement="",x=Value)
      Value  = gsub(pattern="m$"      ,replacement="",x=Value)
      Value  = gsub(pattern="n$"      ,replacement="",x=Value)
      Value  = gsub(pattern="m.$"     ,replacement="",x=Value)
      #---~---


      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---


      #--- Make sure all data have the same units (cm). Discard percent data
      um_sel         = ( UnitOrig %in% c("micro m","microm","um") ) | has_um
      dmm_sel        = ( UnitOrig %in% c("0.1mm")                 )
      mm_sel         = ( UnitOrig %in% c("mm","millimiter")       ) | has_mm
      cm_sel         = ( UnitOrig %in% c("(cm)","cm")             ) | has_cm
      dm_sel         = ( UnitOrig %in% c("dm")                    ) | has_dm
      in_sel         = ( UnitOrig %in% c("in","inch","inches")    ) | has_in
      ft_sel         = ( UnitOrig %in% c("ft","feet")             ) | has_ft
      m_sel          = ( UnitOrig %in% c("m","meter")             ) | has_m
      Value[um_sel ] = as.character( um.2.cm       * as.numeric(Value[um_sel ]) )
      Value[dmm_sel] = as.character( 0.1 * mm.2.cm * as.numeric(Value[dmm_sel]) )
      Value[mm_sel ] = as.character(       mm.2.cm * as.numeric(Value[mm_sel ]) )
      Value[in_sel ] = as.character(       in.2.cm * as.numeric(Value[in_sel ]) )
      Value[dm_sel ] = as.character(       dm.2.cm * as.numeric(Value[dm_sel ]) )
      Value[ft_sel ] = as.character(       ft.2.cm * as.numeric(Value[ft_sel ]) )
      Value[m_sel  ] = as.character(        m.2.cm * as.numeric(Value[m_sel  ]) )
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( um_sel  | dmm_sel | mm_sel  | cm_sel  | dm_sel  | in_sel
                           | ft_sel  | m_sel   )
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---
   }else if (TraitID %in% c(7L)){
      #---~---
      #   7 - Mycorrhiza type
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---



      #---~---
      #   We define four classes of mycorrhizal association (plus one class for 
      # non-mycorrhiza species), based on van der Heijdeen et al. (2015)
      # https://dx.doi.org/10.1111/nph.13288
      #---~---
      IsAbsent         = rep(FALSE   ,times=length(NameOrig))
      IsEctomycorrhiza = rep(FALSE   ,times=length(NameOrig))
      IsArbutoid       = rep(FALSE   ,times=length(NameOrig))
      IsEndomycorrhiza = rep(FALSE   ,times=length(NameOrig))
      IsArbuscular     = rep(FALSE   ,times=length(NameOrig))
      IsEricoid        = rep(FALSE   ,times=length(NameOrig))
      IsOrchid         = rep(FALSE   ,times=length(NameOrig))
      IsMonotropoid    = rep(FALSE   ,times=length(NameOrig))
      #---~---



      #---~---
      #   Some authors ancillary information that describe their classification strength.
      # Discard these data.
      #---~---
      #--- Colleen Iversen
      IsTMI        =
         ( ( AuthorName %in% "Colleen Iversen" )
         & ( NameOrig   %in% c( "mycorrhiza_type_maherali")
           )#end NameOrig
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Nadejda Soudzilovskaia
      IsTMI        =
         ( ( AuthorName %in% "Nadejda Soudzilovskaia" )
         & ( NameOrig   %in% c( "original term for mycorrhizal type given by selivanov")
           )#end NameOrig
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Gijsbert Werner
      IsTMI        =
         ( ( AuthorName %in% "Gijsbert Werner" )
         & ( NameOrig   %in% c( "alternative", "am_inferred", "am_lost_likelihood"
                              , "am_retained_likelihood", "am_stable_likelihood"
                              ,"labile_likelihood", "stable_am_loss_likelihood"
                              )#end c
           )#end NameOrig
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---



      #---~---
      #   Translate classifications from various authors to a unified system.
      #---~---
      #---~---
      #   F Stuart Chapin III, their classification does not distinguish between 
      # mycorrhiza types, keep only the non-mycorrhiza data.
      #---~---
      IsAuthor       =
         ( AuthorName %in% "F Stuart Chapin III" ) & ( NameOrig %in% "myco" )
      IsAbsent       = IsAbsent | ( IsAuthor & (Value %in% "0") )
      Discard        = IsAuthor & ( ! ( Value %in% "0") )
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #--- Johannes Cornelissen, keep all but the mixed classes.
      IsAuthor       =
         ( ( AuthorName %in% "Johannes Cornelissen"                      )
         & ( NameOrig   %in% c("mycorrhizza type", "mycorrhizal type"  ) )
         )#end IsAuthor
      IsAbsent         = IsAbsent         | ( IsAuthor & (Value %in% c("n"           )) )
      IsEctomycorrhiza = IsEctomycorrhiza | ( IsAuthor & (Value %in% c("ecto","ec"   )) )
      IsEricoid        = IsEricoid        | ( IsAuthor & (Value %in% c("ericoid","er")) )
      IsArbuscular     = 
         IsArbuscular | ( IsAuthor & (Value %in% c("am","forb-am","gram-am","wood-am")) )
      Discard          = IsAuthor & ( Value %in% c("nm/am", "ec/am") )
      Value[Discard]   = NA_character_
      Valid[Discard]   = FALSE
      VName[Discard]   = NA_character_
      #--- Joseph Craine, keep all but the mixed classes.
      IsAuthor       =
         ( AuthorName %in% "Joseph Craine" ) & ( NameOrig %in% "mycorrhizal guess" )
      IsAbsent         = IsAbsent         | ( IsAuthor & (Value %in% c("non"    )) )
      IsEctomycorrhiza = IsEctomycorrhiza | ( IsAuthor & (Value %in% c("ecto"   )) )
      IsEricoid        = IsEricoid        | ( IsAuthor & (Value %in% c("ericoid")) )
      IsArbuscular     = IsArbuscular     | ( IsAuthor & (Value %in% c("am"     )) )
      #---~---
      #   Ian Dickie, their classification does not distinguish between non-mycorrhiza
      # and endomycorrhiza. Keep only the ectomycorrhiza data.
      #---~---
      IsAuthor       =
         ( AuthorName %in% "Ian Dickie" ) & ( NameOrig %in% "ectomycorrhizal" )
      IsEctomycorrhiza = IsEctomycorrhiza | ( IsAuthor & (Value %in% c("ectomycorrhizal")))
      Discard          = IsAuthor & ( Value %in% c("non-ectomycorrhizal") )
      Value[Discard]   = NA_character_
      Valid[Discard]   = FALSE
      VName[Discard]   = NA_character_
      #--- Henry Ford, keep all but mixed classes.
      IsAuthor       =
         ( AuthorName %in% "Henry Ford" ) & ( NameOrig %in% "mycorrhiza" )
      IsAbsent         = IsAbsent         | ( IsAuthor & (Value %in% c("absent"     )) )
      IsEctomycorrhiza = IsEctomycorrhiza | ( IsAuthor & (Value %in% c("ec?","ecto" )) )
      IsArbutoid       = 
         IsArbutoid       | ( IsAuthor & (Value %in% c("arbutoid","pyroloid"        )) )
      IsEndomycorrhiza = IsEndomycorrhiza | ( IsAuthor & (Value %in% c("endo"       )) )
      IsArbuscular     = 
         IsArbuscular     | ( IsAuthor & (Value %in% c("?va","arbuscular","va","va?")) )
      IsEricoid        = IsEricoid        | ( IsAuthor & (Value %in% c("ericoid"    )) )
      IsOrchid         = IsOrchid         | ( IsAuthor & (Value %in% c("orchid"     )) )
      IsMonotropoid    = IsMonotropoid    | ( IsAuthor & (Value %in% c("monotropoid")) )
      Discard        = 
         IsAuthor & ( Value %in% c("ectendo", "ecto; ectendo", "ecto; endo; va") )
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #--- Colleen Iversen, keep all but mixed classes.
      IsAuthor       =
         ( AuthorName %in% "Colleen Iversen" ) & ( NameOrig %in% "mycorrhiza_type" )
      IsAbsent         = IsAbsent         | ( IsAuthor & (Value %in% c("nm"  )) )
      IsEctomycorrhiza = IsEctomycorrhiza | ( IsAuthor & (Value %in% c("em"  )) )
      IsArbutoid       = IsArbutoid       | ( IsAuthor & (Value %in% c("abtm")) )
      IsArbuscular     = IsArbuscular     | ( IsAuthor & (Value %in% c("am"  )) )
      IsEricoid        = IsEricoid        | ( IsAuthor & (Value %in% c("erm" )) )
      IsOrchid         = IsOrchid         | ( IsAuthor & (Value %in% c("orm" )) )
      Discard          = IsAuthor & ( Value %in% c("am + em", "ds", "eem") )
      Value[Discard]   = NA_character_
      Valid[Discard]   = FALSE
      VName[Discard]   = NA_character_
      #--- Renske Onstein, mycorrhiza classification is binary. Keep only the negatives.
      IsAuthor       =
         ( AuthorName %in% "Renske Onstein" ) & ( NameOrig %in% "mycorrhiza" )
      IsAbsent         = IsAbsent         | ( IsAuthor & ( Value %in% "no" ) )
      Discard          = IsAuthor & ( ! ( Value %in% "no") )
      Value[Discard]   = NA_character_
      Valid[Discard]   = FALSE
      VName[Discard]   = NA_character_
      #--- Josep Penuelas, a single data entry exists (an ericoid mycorrhiza).
      IsAuthor       =
         ( AuthorName %in% "Josep Penuelas" ) & ( NameOrig %in% "mycorrhizal type" )
      IsEricoid        = IsEricoid        | ( IsAuthor & (Value %in% c("ericoid") ) )
      #--- Colleen Seymour, they used binary flags for ectomycorrhiza (no further classes)
      IsAuthor         =
         ( ( AuthorName %in% "Colleen Seymour"                            )
         & ( NameOrig   %in% "nutrient uptake strategy (ectomycorrhizae)" )
         )#end IsAuthor
      IsEctomycorrhiza = IsEctomycorrhiza | ( IsAuthor & (Value %in% c("1")) )
      Discard          = IsAuthor & ( Value %in% "0")
      Value[Discard]   = NA_character_
      Valid[Discard]   = FALSE
      VName[Discard]   = NA_character_
      #--- Nadejda Soudzilovskaia, keep all but mixed classes.
      IsAuthor       =
         ( ( AuthorName %in% "Nadejda Soudzilovskaia"                )
         & ( NameOrig %in% "modern term for type of mycorrhiza type" )
         )#end IsAuthor
      IsAbsent         = IsAbsent         | ( IsAuthor & (Value %in% c("no"               )) )
      IsEctomycorrhiza = IsEctomycorrhiza | ( IsAuthor & (Value %in% c("ectomycorrhiza"   )) )
      IsArbutoid       = IsArbutoid       | ( IsAuthor & (Value %in% c("arbutoid"         )) )
      IsEndomycorrhiza = IsEndomycorrhiza | ( IsAuthor & (Value %in% c("endo-unidentified")) )
      IsArbuscular     = IsArbuscular     | ( IsAuthor & (Value %in% c("am"  )) )
      IsEricoid        = IsEricoid        | ( IsAuthor & (Value %in% c("ericoid" )) )
      IsOrchid         = IsOrchid         | ( IsAuthor & (Value %in% c("orchid" )) )
      Discard          = IsAuthor & ( Value %in% c("ds") )
      Value[Discard]   = NA_character_
      Valid[Discard]   = FALSE
      VName[Discard]   = NA_character_
      #--- Oscar Valverde-Barrantes, keep all but mixed classes.
      IsAuthor       =
         ( ( AuthorName %in% "Oscar Valverde-Barrantes" )
         & ( NameOrig %in% "mycorrhizal_affiliation"    )
         )#end IsAuthor
      IsAbsent         = IsAbsent         | ( IsAuthor & (Value %in% c("nm"      )) )
      IsEctomycorrhiza = IsEctomycorrhiza | ( IsAuthor & (Value %in% c("ecm"     )) )
      IsArbuscular     = IsArbuscular     | ( IsAuthor & (Value %in% c("am"      )) )
      IsEricoid        = IsEricoid        | ( IsAuthor & (Value %in% c("ericoid" )) )
      Discard          = IsAuthor & ( Value %in% "noinf")
      Value[Discard]   = NA_character_
      Valid[Discard]   = FALSE
      VName[Discard]   = NA_character_
      #--- Gijsbert Werner, they use a system of Boolean variables.
      IsAuthor          = ( AuthorName %in% "Gijsbert Werner" ) & ( Value %in% "yes" )
      IsArbuscular      = IsArbuscular     | ( IsAuthor & ( NameOrig %in% c("am" ) ) )
      IsArbutoid        = IsArbutoid       | ( IsAuthor & ( NameOrig %in% c("arb") ) )
      IsEctomycorrhiza  = IsEctomycorrhiza | ( IsAuthor & ( NameOrig %in% c("ecm") ) )
      IsEricoid         = IsEricoid        | ( IsAuthor & ( NameOrig %in% c("er" ) ) )
      IsAbsent          = IsAbsent         | ( IsAuthor & ( NameOrig %in% c("nm" ) ) )
      IsOrchid          = IsOrchid         | ( IsAuthor & ( NameOrig %in% c("orm") ) )
      Value[IsAuthor]   = NA_character_
      Discard           = 
         ( ( AuthorName %in% "Gijsbert Werner" ) 
         & ( ( Value %in% "no" ) | ( NameOrig %in% c("any_non_am_mf") ) )
         )#end Discard
      Value[Discard]    = NA_character_
      Valid[Discard]    = FALSE
      VName[Discard]    = NA_character_
      #---~---

      #---~---
      #   Assign standardised classes (multiple pathways separated by dashes).
      #---~---
      Value[IsAbsent        ] = "Absent"
      Value[IsEndomycorrhiza] = "Endomycorrhiza (not specified)"
      Value[IsArbutoid      ] = "Arbutoid"
      Value[IsArbuscular    ] = "Arbuscular"
      Value[IsEricoid       ] = "Ericoid"
      Value[IsOrchid        ] = "Orchid"
      Value[IsMonotropoid   ] = "Monotropois"
      Value[IsEctomycorrhiza] = "Ectomycorrhiza"
      #---~---



      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( IsAbsent  | IsEndomycorrhiza | IsArbutoid    | IsArbuscular 
                           | IsEricoid | IsOrchid         | IsMonotropoid | IsEctomycorrhiza )
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---

   }else if (TraitID %in% c(8L)){
      #---~---
      #   8   - Plant nitrogen(N) fixation capacity
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---

      #---~---
      #   List of default values.
      #---~---
      NFixStd = c(no="No",yes="Yes")
      #---~---



      #---~---
      #   Madhur Anand. Current download has only non N fixers.
      #---~---
      IsAuthor = ( AuthorName %in% "Madhur Anand")
      Value[IsAuthor & ( Value %in% "none" )] = NFixStd["no"]
      #---~---



      #---~---
      #   The following authors used a binary (0/1) system for N fixation
      # - Chris Baraloto
      # - Mario Liebergesell
      # - Colleen Seymour
      #---~---
      IsAuthor = AuthorName %in% c( "Chris Baraloto", "Mario Liebergesell", "Colleen Seymour"
                                  )#end c
      Value[IsAuthor & ( Value %in% "0" )] = NFixStd["no" ]
      Value[IsAuthor & ( Value %in% "1" )] = NFixStd["yes"]
      #---~---



      #---~---
      #   The following authors used a yes/no system. 
      # - William Bond
      # - Johannes Cornelissen
      # - Joseph Craine
      # - Ning Dong
      # - Jens Kattge
      # - Daniel Laughlin
      # - Maowei Liang
      # - Patrick Meir
      # - Renske Onstein
      # - Josep Penuelas
      # - Hendrik Poorter
      # - Peter Reich
      # - Fiona Soper
      # - Jaime Villacis
      # - Evan Weiher
      # - Ian Wright
      #---~---
      IsAuthor = AuthorName %in% c( "William Bond", "Johannes Cornelissen","Joseph Craine","ning dong"
                                  , "Jens Kattge", "Daniel Laughlin", "Maowei Liang", "Patrick Meir"
                                  , "Renske Onstein", "Josep Penuelas", "Hendrik Poorter", "Peter Reich"
                                  , "Fiona Soper", "JAIME VILLACIS", "Evan Weiher", "Ian Wright"
                                  )#end c
      NoNFixer  = c("n","no" ,"no-n-fixer", "non fixer", "no; not an n fixer", "not n2 fixing"      )
      YesNFixer = c("y","yes",   "n-fixer", "n fixer"  , "yes; an n fixer"   , "n2 fixing"    , "si")
      Discard   = IsAuthor & ( Value %in% c("n2 fixing?") )
      Value[IsAuthor & ( Value %in% NoNFixer  )] = NFixStd["no" ]
      Value[IsAuthor & ( Value %in% YesNFixer )] = NFixStd["yes"]
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---




      #---~---
      #   F Stuart Chapin III. They distinguish between symbiotic and non-symbiotic fixation. We ignore
      # the distinction as most authors don't provide this level of detail.
      #---~---
      IsAuthor = AuthorName %in% "F Stuart Chapin III"
      Value[IsAuthor & ( Value %in% c(    "0") )] = NFixStd["no" ]
      Value[IsAuthor & ( Value %in% c("1","2") )] = NFixStd["yes"]
      #---~---



      #---~---
      #   Will Cornwell. Multiple systems of classification. Simplify categories and remove spurious
      # values.
      #---~---
      IsAuthor = AuthorName %in% "Will Cornwell"
      Value[IsAuthor & ( Value %in% c("n"        ) )] = NFixStd["no" ]
      Value[IsAuthor & ( Value %in% c("y","a","r") )] = NFixStd["yes"]
      Discard = IsAuthor & ( Value %in% c("carn","cyan","hp") )
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---



      #---~---
      #   Walton Green. Classification includes N-fixing grades. Discard "low" as it is unclear if 
      # low includes non-fixers too.
      #---~---
      IsAuthor = AuthorName %in% "Walton Green"
      Value[IsAuthor & ( Value %in% c("medium","high") )] = NFixStd["yes"]
      Discard = IsAuthor & ( Value %in% "low" )
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---



      #---~---
      #   Alvaro G. Gutierrez. TRUE/FALSE system for N fixation
      #---~---
      IsAuthor = AuthorName %in% "Alvaro G. Gutierrez"
      Value[IsAuthor & ( Value %in% "false"  )] = NFixStd["no" ]
      Value[IsAuthor & ( Value %in% "true"   )] = NFixStd["yes"]
      #---~---



      #---~---
      #   Jon Lloyd. They used a nitrogen fixing rank that goes from -1 to 7. The original reference
      # does not refer to the actual value. We assume that 0 means no nitrogen fixation, positive values
      # mean nitrogen fixation, and -1 means not available, and hope for the best...
      #---~---
      IsAuthor = AuthorName %in% "Jon Lloyd"
      Value[IsAuthor & ( Value %in% "0"                          )] = NFixStd["no" ]
      Value[IsAuthor & ( Value %in% as.character(sequence(7L))   )] = NFixStd["yes"]
      Discard        = IsAuthor & ( Value %in% "-1" )
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---



      #---~---
      #   Leho Tedersoo. They used a detailed classification system and provided multiple ancillary data
      # sets. We keep only the consensus, and further simplify the classes. We discard the "unlikely" 
      # classes as they imply low confidence.
      #---~---
      IsAuthor       = ( AuthorName %in% "Leho Tedersoo" ) & ( NameOrig   %in% "consensus estimate" )
      NoNFixer       = c("none")
      YesNFixer      = c("frankia","likely_present","likely_rhizobia","nostocaceae","present","rhizobia")
      MaybeNFixer    = c("unlikely_frankia","unlikely_rhizobia")
      Value[IsAuthor & ( Value %in% NoNFixer    )] = NFixStd["no" ]
      Value[IsAuthor & ( Value %in% YesNFixer   )] = NFixStd["yes"]
      Discard        = 
         ( ( IsAuthor & ( Value %in% MaybeNFixer ) )
         | ( ( AuthorName %in% "Leho Tedersoo" ) & (! ( NameOrig   %in% "consensus estimate" )) )
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---



      #---~---
      #   Gijsbert Werner. They use a no/yes system, but they report the nitrogen fixing
      # habit across multiple variables. We discard the data from the detailed entries and only use the
      # yes/no ones. Not that nfc stands for nitrogen fixing clade, not nitrogen fixation capacity.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Gijsbert Werner" )
                 & ( NameOrig   %in% c("nfix","nitrogen fixation capacity") )
                 )#end IsAuthor
      Value[IsAuthor & ( Value %in% "no"  )] = NFixStd["no" ]
      Value[IsAuthor & ( Value %in% "yes" )] = NFixStd["yes"]
      Discard        = ( ( AuthorName %in% "Gijsbert Werner" )
                       & ( NameOrig   %in% c("data_fixing","nfc") )
                       )#end IsAuthor
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Make sure data not assigned to the standard categories become invalid
      #---~---
      IsValid = is.na(Value) | ( Value %in% NFixStd ) 
      Value[! IsValid] = NA_character_
      #---~---



   }else if (TraitID %in% c(12L,33L,59L,155L,770L,1085L,1809L,1955L)){
      #---~---
      #   12   - Leaf lifespan (longevity)
      #   33   - Seed (seedbank) longevity
      #   59   - Plant lifespan (longevity)
      #   155  - Plant ontogeny: age of maturity (first flowering)
      #   770  - Plant mortality rate
      #   1085 - Root persistence (lifespan; longevity)
      #   1809 - Fine root persistence (lifespan; longevity)
      #   1955 - Fine root dry mass turnover rate
      #---~---

      #---~---
      #   Some authors use integer systems to classify plant age. Fix the values for
      # annual/biennial, but discard long-lived plants as it would be too uncertain.
      #---~---
      #   Johannes Cornelissen used the following classification
      # 1 - annual
      # 2 - annual/biennial
      # 3 - biennial
      # 4 - biennial/perennial
      # 5 - perennial"
      #---~---
      IsAuthor           = ( ( AuthorName %in% "Johannes Cornelissen" )
                           & ( NameOrig   %in% "life history" )
                           )#end IsAuthor
      UnitOrig[IsAuthor] = "year"
      Value[IsAuthor & ( Value %in% c("1")    )] = "1"
      Value[IsAuthor & ( Value %in% c("2")    )] = "1-2"
      Value[IsAuthor & ( Value %in% c("3")    )] = "2"
      Discard        = IsAuthor & ( Value %in% c("4","5"))
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---
      #   Marco Moretti used the following classification
      #   1 - annual
      #   2 - usually biennial
      #   3 - perennial
      #---~---
      IsAuthor = ( AuthorName %in% "Marco Moretti" ) & ( NameOrig   %in% "life history" )
      UnitOrig[IsAuthor] = "year"
      Value[IsAuthor & ( Value %in% c("1")    )] = "1"
      Value[IsAuthor & ( Value %in% c("2")    )] = "1-2"
      Discard        = IsAuthor & ( Value %in% c("3"))
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Discard binary data that do not provide age range
      #---~---
      Discard        = ( AuthorName %in% "Alexandra Weigelt" ) & ( NameOrig   %in% "flowering" )
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Some authors provided multiple statistics for value. Eliminate most of them to
      # avoid biases.
      #---~---
      IsAuthor = 
         ( ( AuthorName %in% "Colleen Iversen" )
         & ( NameOrig   %in% c( "lower quartile_root turnover_annual root system replacement"
                              , "lower quartile_specific root length"
                              , "max_root turnover_annual root system replacement"
                              , "median_specific root length (srl)"
                              , "min_root turnover_annual root system replacement"
                              , "root turnover_annual root system replacement"
                              , "upper quartile_root turnover_annual root system replacement"
                              , "upper quartile_specific root length"
                              )#end c
           )#end NameOrig
         )#end IsAuthor
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---

      #---~---
      #   Discard bad values, either odd units, or if longevity or turnover rate are
      # zero, which makes no sense.
      #---~---
      bad_sel        = UnitOrig %in% c("text","m/g")
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #--- Remove spurious characters (but track those with units in the value).
      has_day = ( grepl(pattern="d$"     ,x=Value)
                | grepl(pattern="day.$"  ,x=Value) )
      has_wk  = ( grepl(pattern="w$"     ,x=Value)
                | grepl(pattern="wk$"    ,x=Value)
                | grepl(pattern="wk.$"   ,x=Value)
                | grepl(pattern="week$"  ,x=Value)
                | grepl(pattern="week.$" ,x=Value) )
      has_mon = ( grepl(pattern="m$"     ,x=Value)
                | grepl(pattern="mon$"   ,x=Value)
                | grepl(pattern="mon.$"  ,x=Value)
                | grepl(pattern="month$" ,x=Value)
                | grepl(pattern="month.$",x=Value) )
      has_yr  = ( grepl(pattern="y$"     ,x=Value)
                | grepl(pattern="yr$"    ,x=Value)
                | grepl(pattern="yr.$"   ,x=Value)
                | grepl(pattern="year$"  ,x=Value)
                | grepl(pattern="year.$" ,x=Value)
                | grepl(pattern="years.$",x=Value) )
      Value   = gsub(pattern=" "      ,replacement="",x=Value)
      Value   = gsub(pattern="d$"     ,replacement="",x=Value)
      Value   = gsub(pattern="day.$"  ,replacement="",x=Value)
      Value   = gsub(pattern="w$"     ,replacement="",x=Value)
      Value   = gsub(pattern="wk$"    ,replacement="",x=Value)
      Value   = gsub(pattern="wk.$"   ,replacement="",x=Value)
      Value   = gsub(pattern="week$"  ,replacement="",x=Value)
      Value   = gsub(pattern="week.$" ,replacement="",x=Value)
      Value   = gsub(pattern="m$"     ,replacement="",x=Value)
      Value   = gsub(pattern="mon$"   ,replacement="",x=Value)
      Value   = gsub(pattern="mon.$"  ,replacement="",x=Value)
      Value   = gsub(pattern="month$" ,replacement="",x=Value)
      Value   = gsub(pattern="month.$",replacement="",x=Value)
      Value   = gsub(pattern="y$"     ,replacement="",x=Value)
      Value   = gsub(pattern="yr$"    ,replacement="",x=Value)
      Value   = gsub(pattern="yr.$"   ,replacement="",x=Value)
      Value   = gsub(pattern="year$"  ,replacement="",x=Value)
      Value   = gsub(pattern="year.$" ,replacement="",x=Value)
      Value   = gsub(pattern="years.$",replacement="",x=Value)
      #---~---


      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---

      #---~---
      #   Dominik Thom provided fraction of total fine root biomass lost per year. We 
      # can transform this in decay rate, and invert it to report data in years.
      #---~---
      IsAuthor = 
         ( (TraitID %in% 1955L ) & (AuthorName %in% "Dominik Thom")
         & ( NameOrig %in% "root turnover rate")
         )#end IsAuthor
      Value   [IsAuthor] = as.character(- log(1. - as.numeric(Value[IsAuthor])))
      UnitOrig[IsAuthor] = "yr-1"
      #---~---


      #---~---
      #   Discard zero values, as neither zero longevity nor turnover rate make sense.
      #---~---
      bad_zero        = as.numeric(Value) %eq% 0.
      Value[bad_zero] = NA_character_
      Valid[bad_zero] = FALSE
      VName[bad_zero] = NA_character_
      #---~---


      #--- Make sure all data have the same units (year).
      day_sel           = UnitOrig %in% c("d","day","days")                       | has_day
      week_sel          = UnitOrig %in% c("w","wk","wks","week","weeks")          | has_wk
      mon_sel           = UnitOrig %in% c("mo","month","months")                  | has_mon
      year_sel          = UnitOrig %in% c("y","yr","yrs","year","years","years?") | has_yr
      oneoyr_sel        = UnitOrig %in% c("yr-1")
      Value[day_sel   ] = as.character( 1./yr.day  * as.numeric(Value[day_sel   ]))
      Value[day_sel   ] = as.character( 1./yr.day  * as.numeric(Value[day_sel   ]))
      Value[week_sel  ] = as.character( 1./yr.week * as.numeric(Value[week_sel  ]))
      Value[mon_sel   ] = as.character( 1./yr.mon  * as.numeric(Value[mon_sel   ]))
      Value[oneoyr_sel] = as.character( 1.         / as.numeric(Value[oneoyr_sel]))
      #---~---


      #---~---
      #   Discard data that has no units either in the value or in the units
      #---~---
      miss_unit        = is.na(UnitOrig) & ( ! ( has_day | has_wk | has_mon | has_yr ) )
      Value[miss_unit] = NA_character_
      Valid[miss_unit] = FALSE
      VName[miss_unit] = NA_character_
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( day_sel | week_sel | mon_sel | year_sel | oneoyr_sel )
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---

   }else if (TraitID %in% c(22L)){
      #---~---
      #   Leaf photosynthesis pathway
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---


      #---~---
      #   Discard meaningless data
      #---~---
      IsInvalid        = 
         tolower(Value) %in% c("?","??","no","shrub","unknown","tbc"
                              ,"http://tropical.theferns.info/viewtropical.php?id=vochysia+haenkeana"
                              )#end c
      Value[IsInvalid] = NA_character_
      Valid[IsInvalid] = FALSE
      VName[IsInvalid] = NA_character_
      #---~---


      #---~---
      #   Some original names are "yes/no" questions. Replace then with classes.
      #---~---
      IsC3         = ( ( NameOrig %in% "c3"  ) & ( Value %in% c("y","yes")) )
      IsC4         = ( ( NameOrig %in% "c4"  ) & ( Value %in% c("y","yes")) )
      IsCAM        = ( ( NameOrig %in% "cam" ) & ( Value %in% c("y","yes")) )
      Value[IsC3 ] = "c3"
      Value[IsC4 ] = "c4"
      Value[IsCAM] = "cam"
      #---~---



      #---~---
      #   Decide photosynthetic pathway
      #---~---
      IsC3      = Value %in% c("3","c3","c3?","c3.")
      IsC4      = Value %in% c("c4","c4?")
      IsCAM     = Value %in% c("cam","cam?")
      IsC3C4    = Value %in% c("c3/c4")
      IsC3CAM   = Value %in% c("c3/cam", "c3-cam")
      IsC4CAM   = Value %in% c("c4/cam", "c4-cam")
      IsC3C4CAM = Value %in% c("c3/c4/cam")
      #---~---

      #---~---
      #   Assign standardised classes (multiple pathways separated by dashes).
      #---~---
      Value[IsC3     ] = "C3"
      Value[IsC4     ] = "C4"
      Value[IsCAM    ] = "CAM"
      Value[IsC3C4   ] = "C3-C4"
      Value[IsC3CAM  ] = "C3-CAM"
      Value[IsC4CAM  ] = "C4-CAM"
      Value[IsC3C4CAM] = "C3-C4-CAM"
      #---~---



      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( IsC3 | IsC4 | IsCAM | IsC3C4 | IsC3CAM | IsC4CAM | IsC3C4CAM )
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---

   }else if (TraitID %in% c(28L)){
      #---~---
      #   Dispersal syndrome
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value = tolower(Value)
      #---~---


      #---~---
      #   Simplify the information from Udayangani Liu.
      #---~---
      IsTMI        = 
         ( ( AuthorName %in% "Udayangani Liu" ) 
         & ( NameOrig %in% c( "animalspecies", "disp.mode.animal", "disp.mode.gravity"
                            , "disp.mode.water", "disp.mode.wind", "seed dispersal"
                            , "seed weight notes"
                            )#end c
           )#end NameOrig
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Remove dispersal agent in case it is animal, so the type of animal is accounted.
      IsAuthor =
         ( ( AuthorName %in% "Udayangani Liu" ) 
         & ( NameOrig %in% "postdispersalagentsubgroupdescription" )
         )#end IsAuthor
      IsTMI    = IsAuthor & grepl(pattern="animal",x=Value)
      #---~---


      #---~---
      #   Juli Pausas used a single-letter system. Replace those that are non-overlapping
      # with longer, unambiguous names.
      #---~---
      IsAuthor      = ( AuthorName %in% "Juli Pausas" ) & ( NameOrig %in% c("dispmode") )
      IsSelfAbiotic = IsAuthor & ( Value %in% c("b","bg","g", "gb") )
      IsWind        = IsAuthor & ( Value %in% c("w")         )
      IsWater       = IsAuthor & ( Value %in% c("h")         )
      IsInsect      = IsAuthor & ( Value %in% c("m")         )
      IsAnimal      = IsAuthor & ( Value %in% c("n","o","z", "on") )
      IsAdhesion    = IsAuthor & ( Value %in% c("p")         )
      Discard       = 
         IsAuthor & ( Value %in% c( "gh", "gm", "gw", "hg", "hn", "mb", "mbg", "mg", "ng"
                                  , "nm", "nog", "npg", "nwgp", "og", "ong", "pg", "ph"
                                  , "pmbg", "pmg", "pz","wbg", "wg", "wh", "whg", "wm"
                                  , "wn", "wng", "wp", "wpg", "wpu", "wz", "wzg", "wzh"
                                  , "zb", "zbg", "zg", "zgh", "zhbg", "zhg") )
      Value[IsSelfAbiotic] = "autochory"
      Value[IsWind       ] = "anemochory"
      Value[IsWater      ] = "hydrochory"
      Value[IsInsect     ] = "entomochory"
      Value[IsAnimal     ] = "zoochory"
      Value[IsAdhesion   ] = "epizoochory"
      Value[Discard      ] = NA_character_
      Valid[Discard      ] = FALSE
      VName[Discard      ] = NA_character_
      #---~---


      #---~---
      #   Valerie Raevel used short names. Replace those that are non-overlapping
      # with longer, unambiguous names.
      #---~---
      IsAuthor      = ( ( AuthorName %in% c("Valerie Raevel")      )
                      & ( NameOrig   %in% c("seed dispersal mode") )
                      )#end IsAuthor
      IsSelfAbiotic = IsAuthor & ( Value %in% c("baro") )
      IsWind        = IsAuthor & ( Value %in% c("ane" ) )
      IsAnimal      = IsAuthor & ( Value %in% c("end" ) )
      IsAdhesion    = IsAuthor & ( Value %in% c("epi" ) )
      Value[IsSelfAbiotic] = "barochory"
      Value[IsWind       ] = "anemochory"
      Value[IsAnimal     ] = "zoochory"
      Value[IsAdhesion   ] = "epizoochory"
      #---~---



      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      Discard        = 
         ( ! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE)
         | ( Value %in% c( "agochor", "ane/endo/exo", "ane/hidr", "ane/hidr/endo"
                         , "ball/ane", "ball/ane/hidr", "animal water", "animal wind"
                         , "capsule"
                         , "combination: animal+unassisted", "combination: water+animal"
                         , "combination: water+dispersal prevented"
                         , "combination: water+wind+animal", "combination: wind+animal"
                         , "combination: wind+animal+unassisted"
                         , "combination: wind+unassisted", "combination: wind+water"
                         , "combination: wind+water+unassisted", "developed"
                         , "developed.spatial.dispersal.by.abiotic.vectors"
                         , "developed.spatial.dispersal.by.biotic.vectors"
                         , "diaspore is carried accidentally"
                         , "diaspore is carried intentionally"
                         , "diaspore is carried unintentionally (exo; burr)", "disp"
                         , "dispersal is delayed until some environmental cue"
                         , "dispersal no", "dispersal prevented", "enso", "erosion material"
                         , "external", "fleshy", "fruit is fleshy", "germinule"
                         , "gravity and the caching activities of squirrels and mice are the primary means of dispersal. (1 ..."
                         , "gray squirrels bury and recover the seeds; primarily by wind"
                         , "hemerochor", "hidr/exo", "no", "non specialized"
                         , "none", "other", "passive", "pet"
                         , "primarily wind and some by small mammals"
                         , "primarily wind; red squirrels disperse seeds also"
                         , "restricted.spatial.dispersal", "seed contamination"
                         , "seeds are produced below ground level"
                         , "seeds are produced close to ground level"
                         , "seeds drop to the ground close to or beneath the parent plant"
                         , "shaken fresh water"
                         , "steatornis caripensis; pipile cumanensis; pithecia monachus; cebus albifrons; c. apella; ateles ..."
                         , "unknown", "unspecialised", "vegetative dispersule"
                         , "vegetative growth", "wind animals", "wind water", "wind; insect"
                         , "yes", "water wind animal", "domestic animal"
                         , "combination: methods originating from parent plant+animal"
                         )#end c
           )#end Value
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Decide photosynthetic pathway. For animals, we add some 
      #---~---
      IsHuman       =
         Value %in% c( "car or other vehicle", "commerce", "ethelochor", "farm machinery"
                     , "hay cutting", "hay making machinery", "hay transport", "humans"
                     , "humans.", "man", "mowing", "speirochor", "harvesting"
                     , "clothes and footwear"
                     )#end c
      IsAnimal      =
         Value %in% c( "'birds or other animals'", "'pasture animals'", "a-vertebrate"
                     , "animal", "animal??", "animals", "animals external"
                     , "animals internal"
                     , "anodorhynchus hyacinthinus; deroptyus accipitrinus; tapirus terrestris"
                     , "ants+birds."
                     , "ateuchus squalidus; proechimys ihering; akodon sp.; matachyrus nudicaudatus; phillander opossum ..."
                     , "bat; bird; monkey", "bats; monkeys", "bird; monkey; coatimundi"
                     , "birds and mammals", "birds and mammals  major is bird"
                     , "birds; cattle and horses", "birds/bats."
                     , "cyclura carinata; amazona leucocephala bahamensis"
                     , "dasyprocta sp.; cerdocyon thous; rhes americana; tinamus solitarius"
                     , "deroptyus accipitrinus; philander opossum; caluromys philander"
                     , "diaspore is carried or eaten intentionally depending upon the species of disperser"
                     , "diaspore is eaten accidentally", "diaspore is eaten intentionally"
                     , "diaspore is eaten intentionally; seed ingested or regurgitated"
                     , "dispersal endozoochorous", "dys-zoochory", "dysochor", "earthworm"
                     , "endo", "endo-zoochory", "endo/exo", "endozoochor", "endozoochory"
                     , "for a comprehensive list; see: van rheede van oudtshoorn; k. and van rooyen; m.w. (1999). dispe ..."
                     , "herbivorous mammals; crabs", "herpochor", "hoarding"
                     , "invertebrate", "mammals (bats)+birds."
                     , "mammals (incl. bats); birds; reptiles and fish"
                     , "mammals (incl. bats)+birds.", "mammals (non-bat)+birds."
                     , "mammals (non-bat)+reptiles.", "mammals (unspecified)/birds"
                     , "manure"
                     , "melophorus sp.; rhytidoponera sp.; acanthagenys rufogularis; meliphaga virescens"
                     , "nutcracker; rodents"
                     , "orang utan; rhinocerous; bears; elephant; crocodile"
                     , "ornithochory - mamalochory"
                     , "pan troglodytes troglodytes; cephalophus sylvicultor"
                     , "pan troglodytes troglodytes; mandrillus sphinx"
                     , "pan troglodytes verus; papio papio"
                     , "podarcis pityusensis; martes martes (pine marten); genetta genetta (spotted genet)"
                     , "presbytis potenziani; hylobates mulleri x agilis; cryptophaps poecilorrhoa"
                     , "procyon lotor; sciurus carolinensis; cyanocorax yncas; corvus ossifragus; mimus polyglottos; tu ..."
                     , "pterodorus granulosus; sciurus ingrami; cebus apella; alouatta fusca; leontopithicus chrysopygu ..."
                     , "pteroglossus aracari (toucanet); saguinus midas (tamarin); ateles geoffroyi (spider monkey); ca ..."
                     , "pteropus sp.; ptilinopus magnificus; p. aurantiifrons; ducula spilorrhoa; d. zoeae"
                     , "pteropus sp.; ptilinopus pulchellus; p. superbus; p. iozonus; ducula spilorrhoa"
                     , "ramphastos brevicarinatus; crax sp.; artibeus jamaicensis; ateles geoffroyi; heteromys desmares ..."
                     , "ramphastos spp. (toucans); ateles paniscus (spider monkey); alouatta seniculus (howler monkey); ..."
                     , "ramphastos tucanus; rupicola rupicola; ara sp.; cebus sp.; tayassu sp.; sciurus sp.; agouti sp."
                     , "ramphocelus passerinii; melanerpes chrysauchen; heteromys desmarestianus; hoplomys gymnurus; da ..."
                     , "rhea americana; cerdocyon thous; chrysocyon brachyurus"
                     , "rhea americana; chrysocyon brachyurus", "rodents; varanes; turtles"
                     , "rousettus aegyptiacus; lanius excubitor"
                     , "rupicola rupicola; penelope marail; mazama americana; m. gouazoubira; myleus rhomboidalis"
                     , "rupicola rupicola; phrynops rufipes; tapirus terrestris", "snail"
                     , "sparrow; pig; cow"
                     , "squirrels; many other herbivores; woodpeckers; nutcrackers"
                     , "sylvia atricapilla; s. melanocephala"
                     , "tapirus terrestris; mazama americana; m. gouazoubira; tayassu tajacu; t. pecari; ara ararauna;  ..."
                     , "tapirus terrestris; mazama americana; m. gouazoubira; tayassu tajacu; t. pecari; callicebus tor ..."
                     , "tapirus terrestris; mazama americana; m. gouazoubira; tayassu tajacu; t. pecari; cebus apella;  ..."
                     , "tapirus terrestris; tayassu tajacu; odocoileus virginicus; mazama sp.; dasyprocta leporina; sci ..."
                     , "tayassu pecari; t. tajacu; steatornis caripensis; ramphastos swainsonii"
                     , "toucans; monkey; kinkajou"
                     , "ursu americanus floridanus; aphelocoma coerulescens"
                     , "ursus americanus floridanus; aphelocoma coerulescens"
                     , "ursus americanus floridanus; mimus polyglottos; quiscalus mexicanus; aphelocoma coerulescens; c ..."
                     , "ursus americanus; odocoileus virginianus; procyon lotor; turdus migratorius; bombycilla cedorum"
                     , "varanus grayi; paradoxurus philippinensis", "vertebrate"
                     , "vertebrate-dispersed (aril or fleshy fruit)", "vertebrate?"
                     , "vertebrates", "vertebrates (unspecified).", "water animal", "zoo"
                     , "zoochor", "zoochory", "zoochory with elaiosomes", "birds+fish."
                     , "lizards+mammals (non-bat).", "mammals (bats)+birds"
                     , "mammals (incl. bats); birds and fish"
                     , "mammals (incl. bats); birds and reptiles"
                     , "mammals (incl. bats)+birds", "mammals (non-bat) and reptiles"
                     , "mammals (non-bat)+birds+ants.", "mammals (non-bat)+birds+fish."
                     , "mammals (non-bat)+birds+reptiles.", "mammals (non-bat)+crustacea"
                     , "mammals (non-bat)+invertebrates (non-ant).", "reptiles+birds."
                     )#end c
      IsReptile     = 
         Value %in% c( "antillean terrapin", "climbing iguanas"
                     , "geochelone carbonaria; g. denticulata", "iguanas; alligators"
                     , "lizards", "reptiles.", "terrapene carolina (box turtle)"
                     , "terrapene carolina (eastern box turtle)"
                     , "terrestrial gopher turtles", "testudo elephantopus (tortoise)"
                     , "tropidurus torquatus (lizard)", "turtles", "varanus grayi"
                     )#end c
      IsBird        =
         Value %in% c( "15 species of bird including 6 species of cracidae and 2 species of ramphastidae; ateles panisc ..."
                     , "acridotheres tristis; sturnus contra; s. pagodarum"
                     , "ailuroedus crassirostris; ptilonorhynchus violaceus"
                     , "alectronenas madagascariensis; lepilemur dorsalsis; lemur macaco"
                     , "amazona vittata; margarops fuscatus; columba squamosa; geotrygon montana"
                     , "american robin", "anadorhynchus hyacinthinus"
                     , "anodorhynchus glauca (now extinct)", "anodorhynchus hyacinthinus"
                     , "anthracoceros convexus", "anthracocerus coronatus convexus"
                     , "apterix australis; callaeas cinerea; hemiphaga novaeseelandiae novaeseelandiae"
                     , "ara glaucogularis"
                     , "ara severa; pionites melanocephala; phrynops rufipes"
                     , "aratinga euops", "argusianus argus"
                     , "aulacorhynchus sulcatus sulcatus", "bird", "bird?", "birds"
                     , "birds.", "carried by birds", "cassowary", "casuarinus casuarinus"
                     , "casuarius bennetti (dwarf cassowary)"
                     , "casuarius bennetti pictocollis", "casuarius casuarius"
                     , "casuarius sp.", "cedar waxwing", "ceratogymna atrata"
                     , "ceratogymna atrata; c. cylindricus"
                     , "chamaepestes unicolor (black guan); pharomachrus mocinno (resplendent quetzal); aulacorhynchus  ..."
                     , "cicaeum hirundinaceum (mistletoe bird); acanthogenys rufogularis (spiny-cheeked honeyeater)"
                     , "ciridops anna", "columba leucocephala"
                     , "columba leucocephala; c. squamosa; c. inornata; dendrocygna arborea; d. bicolor; amazona ventra ..."
                     , "coracopsis nigra", "crypturellus boucardi"
                     , "cyanoramphus novaezelandiae"
                     , "daptrius ater; steatornis caripensis; cebus capucinus"
                     , "deroptyus accipitrinus", "dromaius novae-hollandiae (emu)"
                     , "dromaius novaehollandiae (emu)", "ducula aena", "ducula goliath"
                     , "ducula rosacea whartoni", "ducula spilorrhoa"
                     , "ducula spilorrhoa; pteropus alecto"
                     , "ducula spilorrhoa; ptilinopus superbus; casuarius casuarius"
                     , "eaten by birds", "elainea ridleyana (tyrant bird)"
                     , "eudynamis scolopacea", "eudynamys scolopacea"
                     , "finches; tanagers; flycatchers; cotinga", "flycatchers; motmots"
                     , "fruit pigeon", "gracula javanica; turtur tigrinus"
                     , "gypohierax angolensis"
                     , "gypohierax angolensis; tockus fasciatus; ceratogymna elata; c. atrata; corvus albus; merops alb ..."
                     , "icterus sp. (orioles)", "lagopus spp. (grouse; ptarmigan)"
                     , "large birds including toucans", "lopholaimus antarcticus"
                     , "melanerpes carolinus; odocoileus virginianus clavium"
                     , "meliphaga chrysops", "meliphaga virescens", "migrant wood warblers"
                     , "mimus polyglottos", "mimus thenca (chilean mockingbird)"
                     , "mitu salvini (salvin's currasow)"
                     , "myadestes melanops (black-faced solitaire)"
                     , "myadestes melanops (black-faced solitaire); phainoptila melanoxantha (black-and-yellow silky fl ..."
                     , "myadestes townsendi (solitaire)", "myiozetetes simalis"
                     , "nucifraga columbiana (clark's nutcracker)", "ornithochory"
                     , "ortalis ruficauda", "orthopsittaca manilata"
                     , "orthopsittaca manilata; ara manilata; amazona amazonica"
                     , "pan troglodytes troglodytes"
                     , "paradisaea rudolphi (bird of paradise)", "penelope marail"
                     , "phainopepla nitens (phainopepla); melanerpes hypopolius (gila woodpecker); mimus polyglottus (m ..."
                     , "phainoptila melanoxantha sp.; catharus gracilorostris; myadesates melanops; chamaepetes unicolo ..."
                     , "pharomachrus mocinno", "pigeon", "pintail"
                     , "platycercus caledonicus (green rosella)", "primarily bird"
                     , "psarocolius montezuma", "psophia crepitans"
                     , "ptilinopus superbus; p. magnificus"
                     , "ptilonopus magnificus magnificus; lopholaimus antarcticus; ptilonopus regina regina; ailuroedus ..."
                     , "pycnonotus bimaculatus (bulbul)", "pycnonotus cafer; p. luteolus"
                     , "pycnonotus leucogenys mesopotamia; psittacula krameri"
                     , "pycnonotus xanthopygos (bulbul); turdus merula (blackbird)"
                     , "pycnonotus xanthopygos (yellow-vented bulbul); turdus merula (blackbird); sylvia melanocephala  ..."
                     , "ramphastos brevicarinatus"
                     , "ramphastos swainsoni (chestnut-mandibled toucan); guan"
                     , "ramphastos swainsonii; r. sulfuratus; tityra semifasciata; baryphthengus (motmot); trogon masse ..."
                     , "ramphastos swainsonii; tityra semifasciata; vireo flavoviridis"
                     , "ramphastos tucanus", "redwing", "reinwardtoena reinwardsti"
                     , "rose-breasted grosbeak; downy woodpecker; red-eyed vireo; scarlet tanager; veery; cedar waxwing"
                     , "rose-breasted grosbeak; downy woodpecker; red-eyed vireo; scarlet tanager; veery; cedar waxwing ..."
                     , "rupicola rupicola"
                     , "rupicola rupicola; deroptyus accipitrinus; ateles belzebuth"
                     , "selenidera maculirostris", "song thrush"
                     , "strepera fuliginosa (black currawong)", "thraupis aepiscopus"
                     , "tinamou", "toucans", "trichoglossus chlorolepidotus"
                     , "turdus merula (thrush); sylvia atricapilla (blackcap)"
                     , "turdus plebejus (mountain robin); phainoptila melanoxantha (black-and-yellow silky flycatcher); ..."
                     , "turdus viscivorus (mistle thrush)"
                     )#end c
      IsBat         =
         Value %in% c( "artibeus jamaicense", "artibeus jamaicensis"
                     , "artibeus jamaicensis triniatus; a. lituratus palmarum; stearornis caripensis; cebus albifrons;  ..."
                     , "artibeus jamaicensis; eira barbara (tayra); ateles geofroyii (spider monkey)"
                     , "artibeus jamaicensis; glossophaga soricina"
                     , "artibeus lituratus palmarum"
                     , "artibeus lituratus palmarum; steatornis caripensis; thraupis palmarum"
                     , "carollia perspicillata", "eidolon helvum (straw-coloured fruit bat)"
                     , "eidolon sp.; dolonsia sp.; pteropus sp."
                     , "eidolon spp.; rousettus spp.", "fruit bats", "mammals (bats)."
                     , "primarily bat", "pteropus edwardsii", "pteropus sp."
                     , "pteropus vampyrus"
                     )#end c
      IsMammal      =
         Value %in% c( "'probably' pongo pygmaeus", "african elephant", "agouti"
                     , "agouti paca", "agouti paca; dasyprocta punctata"
                     , "agouti; horse; cattle; deer; tapir; peccary; paca; some rodents"
                     , "agouti; peccary; paca. rodents", "alouatta palliata"
                     , "alouatta palliata (howling monkey)", "arboreal mammals"
                     , "ateles belzebuth; tayassu pecari; cebus sp.; callicebus moloch; ateles paniscus; tayassu sp."
                     , "badger", "bat-eared fox; kudu", "bat-eared fox; kudu; sheep"
                     , "bears", "beech marten", "bighorn"
                     , "bos taurus; capra hircus; equus cabalus", "buffalo"
                     , "cacajao melanocephalus melanocephalus; tapirus terrestris; mazama americana; m. gouazoubira; ta ..."
                     , "callicebus torquatus lugens"
                     , "caluromys philander; potos flavus; saguinus midas;"
                     , "canids; primates; bat-eared fox; duiker; kudu"
                     , "canis aureus", "canis latrans (coyote)"
                     , "canis latrans; procyon lotor; ortalis aetula"
                     , "canis latrans; urocyon cinereoargenteus; sialia mexicana; s. currucoides; bombycilla cedrorum"
                     , "carried by mammals", "cattle"
                     , "cattle; horses (occasionally); tapir"
                     , "cebus albifrons", "cebus albifrons; deroptyus accipitrinus"
                     , "cebus albifrons; tayassu tajacu; t. pecari; daptrius ater"
                     , "cebus apella", "cebus apella; c. capucinus; sciurus granatensis"
                     , "cebus sp.; saimiri sciureus; sciurus sp.", "cephalophus callipygus"
                     , "cephalophus callipygus; c. dorsalis"
                     , "cephalophus sylvicultor; c. callipygus"
                     , "cercocebus galeritus galeritus; papio cynocephalus; gypohierax angolensis; tauraco persa"
                     , "cercocebus galeritus; papio cynocephalus"
                     , "cerdocyon thous; procyon cancrivorus", "chamois", "chimpanzee"
                     , "civet", "civet; macaque", "civets?", "cow; goat"
                     , "dasyprocta (agouti)"
                     , "dasyprocta prymnolopha (black-rumped agouti); sciurus aestuans (guianan squirrel)"
                     , "dasyprocta punctata", "dasyprocta punctata (agouti)", "deer"
                     , "deer; rabbit"
                     , "didelphis albiventris; nectomys squamipes; cebus apella; agouti paca; euphractus sexcinctus; da ..."
                     , "didelphus albiventris; nectomys squamipes; cebus apella; turdus sp.; anodorhynchus hyacinthinus"
                     , "dog", "donkey", "dormouse", "eaten by mammals"
                     , "echimys armatus; philander opossum; didelphis marsupialis"
                     , "elephant", "elephant; baboon", "elephant; impala", "elephants"
                     , "elephas maximus", "fox", "fruit bats; civet", "game"
                     , "gazella dorcas (dorcas gazelle); capra nubiana (ibex); goat; camel"
                     , "giraffe; kudu", "goat", "gorilla gorilla gorilla (lowland gorilla)"
                     , "gorilla gorilla; pan troglodytes troglodytes; civettictus civetta; ceratogymna atrata; c. fistu ..."
                     , "hapalemur griseus", "hapalemur griseus occidentalis"
                     , "hapalemur occidentalis; lepilemur dorsalis; lemur macaco"
                     , "hapalemur simus"
                     , "hapalemur simus; ateles belzebuth; colossoma bidens; electrophorus electricus; cotinga ridgwayi ..."
                     , "hare", "hedgehog", "heteromys desmarestianus", "heteromys sp."
                     , "heteromys sp.; chamaepetes unicolor; aulacorhynchus prasinus"
                     , "horse", "horse?; agouti; squirrel; tapir?; peccary?"
                     , "hylobates agilis", "hylobates lar", "kudu; primates"
                     , "lepilemur dorsalis; lemur macaco"
                     , "leptonycteris curasoae; sylvilagus audubonii (desert cottontail); lepus californicus (black-tai ..."
                     , "lepus californicus gray (black-tailed jackrabbit); odocoileus hemionus colombianus (mule deer)"
                     , "livestock", "livestock and wild animals", "livestock; wild mammals"
                     , "loxodonta africana", "loxodonta africana (african elephant)"
                     , "loxodonta africana capensis; papio anubis", "macaca fascicularis"
                     , "macaque", "macaque; civet", "mamalochory", "mammal", "mammalochory"
                     , "mammals (non-bat)", "mammals (non-bat)."
                     , "mammals (non-bat)+humans.", "mammals (unspecified)"
                     , "mandrillus sphinx", "marmot", "marten"
                     , "microcebus murinus; lemur catta; coracopsis vasa", "monkeys"
                     , "monkeys including cebus capucinus and other arboreal mammals including kinkajou"
                     , "mountain hare", "mouse", "mule deer", "myoprocta exilis (acouchy)"
                     , "nasua narica; cebus capucinus; agouti paca; dasyprocta punctata; proechimys semispinosa; sciuru ..."
                     , "nasua narica; cebus capucinus; ateles geoffroyi; dasyprocta punctata; sciurus granatensis; tapi ..."
                     , "odocoileus virginianus clavium", "orycteropus afer (aardvark)"
                     , "ox", "pacarana", "papio anubis (ghanan olive baboon"
                     , "papio ursinus", "paradoxurus (civets)", "paradoxurus hermaphroditus"
                     , "paradoxurus hermaphroditus javanicus"
                     , "paradoxurus hermaphroditus javanicus; hylobates klossii"
                     , "paradoxurus hermaphroditus javanicus; hylobates syndactylus; ducula spilorrhoa; casuarius casua ..."
                     , "paradoxurus hermaphroditus; viverra malaccensis; ducula zoeae; ptilinopus magnificus"
                     , "paradoxurus philippinensis", "pig", "pine marten", "possums"
                     , "potos flaavus; sciurus sp.; cebus capucinus; dasyprocta punctata; heteromys desmarestianus; hop ..."
                     , "potos flavus", "presbytis potenziani"
                     , "presbytis potenziani; pongo pygmaeus; macaca fascicularis"
                     , "primarily mammal"
                     , "proechimys cuvieri; p. guyannensis; myoprocta exilis; dasyprocta leporina; sciurus aestuans"
                     , "rabbit", "rodents", "rodents?; peccary?", "roe"
                     , "reindeer", "ruminants", "saguinus sp.", "sciurus (squirrel)"
                     , "sciurus (squirrels)", "sciurus aureogaster; s. deppei"
                     , "sciurus sp.; dasyprocta sp."
                     , "sciurus spadiceus; cebus apella; dasyprocta variegata; tayassu pecari; proechmys spp.; oryzomys spp."
                     , "sciurus variegatoides; agouti paca; dasyprocta punctata; proechimys semispinosa"
                     , "sheep", "sheep-dummy", "sheep; cattle; horses", "shrew"
                     , "sigmodon hispidus; 'probably' liomys salvini", "small mammals"
                     , "squirrel", "squirrels", "tapirus bairdii"
                     , "tapirus terrestris (tapir)"
                     , "tapirus terrestris; agouti paca; dasyprocta agouti; proechymis ssp.; mazama spp.; sciurus ingra ..."
                     , "taxidea taxus; canis latrans; procyon lotor"
                     , "tayassu tajacu; t. pecari", "ursus americanus floridanus"
                     , "ursus americanus; procyon lotor; odocoileus virginianus; o. v. clavium"
                     , "viverra malaccensis", "vulpes vulpes (red fox)", "wild boar"
                     , "wild cat; jaguar", "mammals (incl. bats)."
                     )#end c
      IsFish        =
         Value %in% c( "brycon (piracanjuba); myloplus (pacu)", "brycon guatemalensis"
                     , "brycon sp.; gymnoderus foetidus", "catfish", "cichlids; pacu"
                     , "colossoma macropomum; c. bidens; brycon sp.; phractocephalus hemelioterus; megaladoras irwini;  ..."
                     , "fish", "fish."
                     )#end c
      IsInsect      =
         Value %in% c( "a-ant", "acromyrmex striatus (leaf-cutting ant)", "ant"
                     , "ant (elaiosome)", "ant-dispersed (elaiosome attached)", "ant?"
                     , "ants", "ants.", "aphaenogaster longiceps"
                     , "aphaenogaster longiceps; pheidole sp."
                     , "aphaenogaster longiceps; rhytidoponera metallica"
                     , "aphenogaster rudis; myrmica punctiventris; formica subsericea; lasius alienus"
                     , "azteca sp.; paratrichima sp. & pheidole sp.", "beetle larva"
                     , "camponotus sp.; chelaner sp.; dolichoderus sp.; iridomyrmex sp.; pheidole sp.; rhytidoponera me ..."
                     , "camponotus sp.; iridomyrmex sp. melophorus spp. pheidole sp.; rhytidoponera spp.; anthochaera c ..."
                     , "entomochory", "harvesting ants", "insects."
                     , "iridomyrmex cf. nitidiceps; pheidole sp."
                     , "iridomyrmex purpureus; pheidole sp.; acanthagenys rufogularis; artamus cinereus; corvus coronoi ..."
                     , "iridomyrmex sp.", "melophorus sp.", "melophorus sp.; pheidole sp."
                     , "messor andrei (harvester ant)", "mirmecochory", "myrmecochory"
                     , "osmatreron bicincta", "pachycondyla harpax; p. apicalis"
                     , "rhytidoponera  tasmaniensis; chelaner cf. rothsteini"
                     , "rhytidoponera ? metallica", "rhytidoponera inornata"
                     , "rhytidoponera metallica; meliphaga chrysops; meliphaga virescens"
                     , "rhytidoponera sp.", "rhytidoponera tasmaniensis"
                     , "rhytidoponera tasmaniensis; iridomyrmex purpureus"
                     , "rhytidoponera tasmaniensis; iridomyrmex sp."
                     , "rhytidoponera violacea", "steartornis caripensis"
                     , "steatornis (oil-bird)", "steatornis caripensis", "trichoptera larva"
                     )#end c
      IsSelfAbiotic =
         Value %in% c( "authochory - mamalochory", "autochor", "autochory", "ball"
                     , "ballistic", "ballistichory", "ballistochory"
                     , "ballistochory/non specialized", "ballochor", "baro", "barochory"
                     , "blastochor"
                     , "diaspore is propelled by action of animal on plant structure"
                     , "diaspore is propelled by action of rain on plant structure"
                     , "diaspore is propelled by action of wind on the plant structure"
                     , "explosive", "explosive mechanism", "generative dispersule"
                     , "gravity", "methods originating from parent plant or diaspore"
                     , "methods originating from parent plant or diaspore; explosive mechanism"
                     , "methods originating from parent plant or diaspore; explosive mechanism;"
                     , "multi-seeded generative dispersule", "nautochor", "ombrochor"
                     , "one-seeded generative dispersule", "pheidole sp."
                     , "pheidole sp.; rhytidoponera metallica", "pheidole spp.", "rainwash"
                     , "tumbling", "unassisted"
                     , "unassisted (no morphological structures aiding dispersal)"
                     , "unassisted and/or methods originating from parent plant"
                     , "unassisted/short-distance"
                     , "wetting by rain or dew"
                     )#end c
      IsAdhesion    =
         Value %in% c( "a-adhesion"
                     , "adhesion-dispersed (hooks; spines or bristles to assist attachment; checked to see if adhered t ..."
                     , "anchorage mechanism/s", "barbed", "diaspore is carried on the outside of the vertebrate"
                     , "dispersal ectozoochorous", "epi-zoochory", "epizoochor"
                     , "epizoochory", "exo/endo", "exozoochory"

                     )#end c
      IsWater       =
         Value %in% c( "(water?)", "(water)", "a-water", "bythisochor"
                     , "floating in fresh- or saltwater currents"
                     , "floating in freshwater currents"
                     , "floating in saltwater currents"
                     , "freshwater currents (not stated whether diaspore floats or sinks)"
                     , "hidr", "hydrochory", "standing fresh water"
                     , "submerged in freshwater currents", "submerged in saltwater currents"
                     , "water", "water?"
                     , "floating or submerged in freshwater currents"
                     )#end c
      IsWind        =
         Value %in% c( "a-wind", "ane", "anemo", "anemochor", "anemochory"
                     , "anemochory: big and round seeds rolling on the ground; pushed by the wind"
                     , "anemochory: big seeds with 'wings'; planing in the air (ex;  acer sp.)"
                     , "anemochory: small seeds with pappus or very light seeds (ex;  crepis sp. or orchis sp.)"
                     , "anemochory: stems move with the wind; helping for seed dispersion (ex;  papaver sp.)"
                     , "boleochor", "chamaechor", "diaspore is blown by wind"
                     , "diaspore is rolled along ground surface by wind", "dispersal wind"
                     , "meteorochor", "rhizomes; wind", "wind", "wind unassisted"
                     , "wind-dispersed (with wing; hairs or bristles to provide air-resistance)"
                     , "wind?", "wind/long-distance"
                     )#end c
      #---~---



      #---~---
      #   Assign standardised classes (multiple pathways separated by dashes).
      #---~---
      Value[IsHuman      ] = "Anthropochory"
      Value[IsAnimal     ] = "Zoochory"
      Value[IsBird       ] = "Ornithochory"
      Value[IsReptile    ] = "Saurochory"
      Value[IsBat        ] = "Chiropterochory"
      Value[IsMammal     ] = "Mammalochory"
      Value[IsFish       ] = "Ichthyochory"
      Value[IsInsect     ] = "Entomochory"
      Value[IsAdhesion   ] = "Epizoochory"
      Value[IsWater      ] = "Hydrochory"
      Value[IsWind       ] = "Anemochory"
      Value[IsSelfAbiotic] = "Autochory"
      #---~---



      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( IsHuman       | IsAnimal      | IsBird        | IsReptile
                           | IsBat         | IsMammal      | IsFish        | IsInsect
                           | IsAdhesion    | IsWater       | IsWind        | IsSelfAbiotic
                           )#end IS
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---

   }else if (TraitID %in% c(29L)){
      #---~---
      #   Pollination syndrome
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value = tolower(Value)
      #---~---


      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      Discard        = 
         ( ! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE)
         | ( Value %in% c( "animals", "animals wind", "autogamous/entomogamous"
                         , "cleistogamy possible", "cleistogamy rare", "cleistogamy unknown"
                         , "geitonogamy at failure of outcrossing", "geitonogamy possible"
                         , "geitonogamy rare", "insects possible", "insects rare"
                         , "insects unknown", "no", "pollination no"
                         , "pseudocleistogamy possible", "pseudocleistogamy rare"
                         , "selfing at failure of outcrossing", "selfing never"
                         , "selfing possible", "selfing rare", "selfing unknown"
                         , "water possible", "water rare", "water unknown", "wind possible"
                         , "wind rare", "wind unknown", "yes"
                         )#end c
           )#end Value
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---



      #---~---
      #   Decide photosynthetic pathway. For animals, we add some 
      #---~---
      IsAnimal  = Value %in% c( "bio", "pollination animals", "slug possible"
                              , "slug rare", "zoophil"
                              )#end c
      IsBat     = Value %in% c( "bat","bats")
      IsBird    = Value %in% c( "bird")
      IsInsect  = Value %in% c( "bee", "bees"
                              , "bees; bumble bees; wasps; bombylides; syrphids"
                              , "bees; bumblebees; wasps; bombylides; syrphids"
                              , "bees; butterflies", "bees; lepidoptera"
                              , "bees; toung < 7 mm", "bees; toung > 7 mm"
                              , "beetles; flies; syrphids; wasps; medium tongued bees"
                              , "beetles; flies; syrphids; wasps; medium tounged bees"
                              , "bumble bees", "bumble bees; lepidoptera"
                              , "bumblebees; butterfflies", "bumblebees; lepidoptera"
                              , "bumblebees; moths", "bumblebees; syrphids"
                              , "butterflies"
                              , "butterflies; long tongued bees; syrphids"
                              , "entomofila", "entomogamous", "entomophil"
                              , "entomophily", "flies", "flies; bees", "flies; beetles"
                              , "general insect", "hymenopteres", "ichneumonidae"
                              , "insect", "insect pollinated", "insects"
                              , "insects (bumblebee)", "insects always", "insects often"
                              , "insects the rule", "lepidoptera; bees"
                              , "lepidoptera; bumble bees", "lepidoptera; flies"
                              , "mixed wind/insect pollinated", "moth/butterfly"
                              , "moths", "moths; hymenoptera"
                              , "short tongued bees; syrphids; flies; beetles"
                              , "short tounged bees; syrphids; muscids; beetles"
                              , "small ichneumonide; flies; beetles", "syrphids"
                              , "syrphids; bees", "very small dipteres", "wasps"
                              )#end c
      IsAbiotic = Value %in% c( "abio")
      IsSelf    = Value %in% c( "autogamous", "cleistogamy often"
                              , "cleistogamy the rule", "geitonogamy often"
                              , "geitonogamy the rule", "pseudocleistogamy often"
                              , "pseudocleistogamy the rule", "self", "selfed"
                              , "selfing always", "selfing often", "selfing the rule"
                              )#end c
      IsWater   = Value %in% c( "hydrogamous", "water", "water always"
                              , "water the rule"
                              )#end c

      IsWind    = Value %in% c( "anemofila", "anemogamous", "anemogamous/entomogamous"
                              , "anemophil", "anemophily", "pollination wind", "wind"
                              , "wind always", "wind often", "wind pollinated"
                              , "wind the rule"
                              )#end c
      #---~---



      #---~---
      #   Assign standardised classes (multiple pathways separated by dashes).
      #---~---
      Value[IsAnimal ] = "Animal"
      Value[IsBat    ] = "Animal/Bats"
      Value[IsBird   ] = "Animal/Birds"
      Value[IsInsect ] = "Animal/Insects"
      Value[IsAbiotic] = "Abiotic"
      Value[IsSelf   ] = "Self"
      Value[IsWater  ] = "Water"
      Value[IsWind   ] = "Wind"
      #---~---



      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( IsAnimal  | IsBird    | IsInsect  | IsAbiotic
                           | IsSelf    | IsWater   | IsWind
                           )#end IS
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---


   }else if(TraitID %in% c(30L,31L,318L,788L)){
      #---~---
      #   Species tolerance to drought
      #   Species tolerance to frost
      #   Species tolerance to fire
      #   Plant light requirement
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---


      #---~---
      #   Remove redundant or too numeric data when some categorical information is 
      # available.
      #---~---
      IsTMI        = 
         ( ( ( AuthorName %in% "Anja Rammig" ) 
           & ( NameOrig %in% c( "fine root", "root", "root cambium", "seedling", "shoot"
                            , "shoot cambium", "stem", "twig"
                            )#end c
             )#end NameOrig
            )#end Anja Rammig
         | ( ( AuthorName %in% "Ross Bradstock" )
           & ( NameOrig   %in% c( "additional data fire regeneration category"
                                , "fire response"
                                , "nfrr regeneration category"
                                , "recommended minimimum interval"
                                )#end c
             )#end NameOrig
           )#end Ross Bradstock
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---


      #---~---
      #   Some authors used numeric systems to assess drought tolerance.
      #---~---
      #--- Ulo Niinemets and Christian Wirth: 0-5 system. 
      IsAuthor = 
         ( ( (AuthorName %in% "Ulo Niinemets"  ) & (NameOrig %in% "drought tolerance"    ) )
         | ( (AuthorName %in% "Christian Wirth") & (NameOrig %in% "traitdroughttolerance") )
         )#end IsAuthor
      suppressWarnings({
         IsHigh         = IsAuthor & ( as.numeric(Value) %gt% 3.333333 )
         IsIntermediate = IsAuthor & ( as.numeric(Value) %wr% c(1.666667,3.333333) )
         IsLow          = IsAuthor & ( as.numeric(Value) %lt% 1.666667 )
      })#end suppressWarnings
      Value[IsHigh        ] = "high"
      Value[IsIntermediate] = "intermediate"
      Value[IsLow         ] = "low"
      #---~--- 
      #   Joseph Craine: Critical potential. Use their threshold (-4.1 MPa) for low 
      # tolerance, and -10 MPa as threshold for high tolerance (based on their data 
      # distribution).
      #---~---
      IsAuthor = ( ( AuthorName %in% "Joseph Craine" )
                 & ( NameOrig %in% c("psicrit","psicrit (mpa)") )
                 )#end IsAuthor
      suppressWarnings({
         IsHigh         = IsAuthor & ( abs(as.numeric(Value)) %gt% 10.        )
         IsIntermediate = IsAuthor & ( abs(as.numeric(Value)) %wr% c(4.1,10.) )
         IsLow          = IsAuthor & ( abs(as.numeric(Value)) %lt% 4.1        )
      })#end suppressWarnings
      #---~---


      #---~---
      #   Some authors used numeric systems to assess frost tolerance.
      #---~---
      #--- Anja Rammig: use the USDA Plant Hardiness Zone.
      IsAuthor = 
         ( (AuthorName %in% "Anja Rammig") & (NameOrig %in% "usda frost hardiness zone") )
      suppressWarnings({
         IsHigh         = IsAuthor & ( as.integer(Value) %in% c( 1L, 2L, 3L, 4L, 5L ) )
         IsIntermediate = IsAuthor & ( as.integer(Value) %in% c( 6L, 7L, 8L, 9L     ) )
         IsLow          = IsAuthor & ( as.integer(Value) %in% c(10L,11L,12L,13L     ) )
      })#end suppressWarnings
      Value[IsHigh        ] = "high"
      Value[IsIntermediate] = "intermediate"
      Value[IsLow         ] = "low"
      #--- Bradford Hawkins: use temperature (degC) and USDA Plant Hardiness Zone as guiding.
      IsAuthor = ( (AuthorName %in% "Bradford Hawkins")
                 & (NameOrig %in% "cold tolerance (degrees c)")
                 )#end IsAuthor
      suppressWarnings({
         IsHigh         = IsAuthor & ( as.numeric(Value) %lt% -23.3         )
         IsIntermediate = IsAuthor & ( as.numeric(Value) %wr% c(-23.3,-1.1) )
         IsLow          = IsAuthor & ( as.numeric(Value) %gt% -1.1          )
      })#end suppressWarnings
      #--- Walton Green: use temperature (degF) and USDA Plant Hardiness Zone as guiding.
      IsAuthor = 
         ( (AuthorName %in% "Walton Green") & (NameOrig %in% "temperature; minimum") )
      suppressWarnings({
         IsHigh         = IsAuthor & ( as.numeric(Value) %lt% -10.        )
         IsIntermediate = IsAuthor & ( as.numeric(Value) %wr% c(-10.,30.) )
         IsLow          = IsAuthor & ( as.numeric(Value) %gt% 30.         )
      })#end suppressWarnings
      #---~---


      #---~---
      #   Some authors used numeric systems to assess fire tolerance.
      #---~---
      #--- Ross Bradstock.  Use the sensitivity flags (4 and 5 are NA).
      IsAuthor =  ( ( AuthorName %in% "Ross Bradstock" ) 
                  & ( NameOrig %in% c("sensitivity: frequent","sensitivity: infrequent") )
                  )#end IsAuthor
      suppressWarnings({
         IsHigh         = IsAuthor & ( as.integer(Value) %in% c(3L) )
         IsIntermediate = IsAuthor & ( as.integer(Value) %in% c(2L) )
         IsLow          = IsAuthor & ( as.integer(Value) %in% c(1L) )
      })#end suppressWarnings
      Value[IsHigh        ] = "high"
      Value[IsIntermediate] = "intermediate"
      Value[IsLow         ] = "low"
      #--- F. Stuart III Chapin.  Use the survivorship flags (4 and 5 are NA).
      IsAuthor =  
         ( ( AuthorName %in% "F. Stuart III Chapin" ) & ( NameOrig %in% c("fire_surv") ) )
      suppressWarnings({
         IsHigh         = IsAuthor & ( as.integer(Value) %in% c(3L) )
         IsIntermediate = IsAuthor & ( as.integer(Value) %in% c(2L) )
         IsLow          = IsAuthor & ( as.integer(Value) %in% c(0L,1L) )
      })#end suppressWarnings
      Value[IsHigh        ] = "high"
      Value[IsIntermediate] = "intermediate"
      Value[IsLow         ] = "low"
      #--- Tianhua He.  Set fire resistance based on the sprouting information
      IsAuthor =  
         ( ( AuthorName %in% "Tianhua He" ) 
         & ( NameOrig %in% "e - epicormic:1; l - lignotuber:2; e and l: 3; r - rhizome:4; e and ;non:0" )
         )#end IsAuthor
      IsHigh         = IsAuthor & ( Value %in% c( "epicormic sprouting"
                                                , "lignotuberous and epicormic sprouting"
                                                , "lignotuberous sprouting"
                                                , "rhizome"
                                                )#end c
                                  )#end Value
      IsLow          = IsAuthor & (Value %in% c("non-sprouting") )
      Value[IsHigh        ] = "high"
      Value[IsLow         ] = "low"
      #--- Michelle Leishman. Author provided fire mortality, need to swap information.
      IsAuthor =  
         ( (AuthorName %in% "Michelle Leishman") & (NameOrig %in% "fm; fire mortality") )
      IsHigh         = IsAuthor & ( Value %in% c("low") )
      IsIntermediate = IsAuthor & ( Value %in% c("moderate") )
      IsLow          = IsAuthor & ( Value %in% c("total (i.e. annuals)","high") )
      Value[IsHigh        ] = "high"
      Value[IsIntermediate] = "intermediate"
      Value[IsLow         ] = "low"
      #--- Mario Liebergesell. No fire tolerance is reported as zero, which would become NA.
      IsAuthor =  
         ( ( AuthorName %in% "Mario Liebergesell" ) & ( NameOrig %in% "fire.tol" ) )
      IsHigh         = IsAuthor & ( Value %in% c("high"   ) )
      IsIntermediate = IsAuthor & ( Value %in% c("medium" ) )
      IsLow          = IsAuthor & ( Value %in% c("0","low") )
      Value[IsHigh        ] = "high"
      Value[IsIntermediate] = "intermediate"
      Value[IsLow         ] = "low"
      #--- Juli Pausas. Harmonise quantitative and qualitative labels.
      IsAuthor =  
         ( ( AuthorName %in% "Juli Pausas" ) & ( NameOrig %in% "seedlsurv" ) )
      suppressWarnings({
         IsHigh         = IsAuthor & ( (as.numeric(Value) %ge% 25.) | (Value %in% "high") )
         IsLow          = IsAuthor & ( (as.integer(Value) %lt% 25.) | (Value %in% "low" ) )
      })#end suppressWarnings
      Value[IsHigh        ] = "high"
      Value[IsLow         ] = "low"
      #--- Maowei Liang. Rename drought tolerance to remove ambiguity
      IsAuthor =
         ( ( AuthorName %in% "Maowei Liang") & ( NameOrig %in% "water_ecotypes" ) )
      IsHigh         = IsAuthor & ( Value %in% c("xerophyte"      ) )
      IsIntermediate = IsAuthor & ( Value %in% c("mesoxerophytes" ) )
      Value[IsHigh        ] = "high"
      Value[IsIntermediate] = "intermediate"
      #---~---


      #---~---
      #   Some input variables are ancillary or refer to classification algorithms or
      # information that is too detailed to retain. We discard these values.
      #---~---
      IsTMI = 
         NameOrig %in% c( "fire regime", "seedlemerg", "fire resistant", "fire.res")
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---




      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      Discard        = ( ! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE)
                       | ( Value %in% c( "wilts", "no", "unknown", "variable"
                                       , "resp", "seed", "total (i.e. annuals)", "r", "r s"
                                       , "s", "s r"
                                       )#end c
                         )#end Value
                       )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Reduce the number of classes for simplicity.
      #   In the case of shade tolerance, the original trait is light requirement, we
      # assume full-light plants are shade intolerant.
      #---~---
      IsHigh         = Value %in% c( "high"
                                   , "very drought resistant"
                                   , "hardy"
                                   , "very resistant"
                                   , "fairly drought resistant"
                                   , "high - intermediate"
                                   , "resists"
                                   , "resistant"
                                   , "yes"
                                   , "fire resistant"
                                   , "plant of very shadowy sites"
                                   )#end c
      IsIntermediate = Value %in% c( "intermediate"
                                   , "medium"
                                   , "wilts and recovers"
                                   , "moderate"
                                   , "medium; dies off after several months"
                                   , "plant of half-shadowy sites"
                                   , "freezingexposed"
                                   )#end c
      IsLow          = Value %in% c( "intermediate - low"
                                   , "freezingunexposed"
                                   , "sensitive"
                                   , "low"
                                   , "no"
                                   , "none"
                                   , "none; dies off in dry conditions"
                                   , "wilts and dies"
                                   , "wilts and dies back"
                                   , "not fire resistant"
                                   , "plant of full-light"
                                   )#end c
      #---~---






      #---~---
      #   Reduce the number of classes for simplicity. Add numbers so they are sorted.
      #---~---
      Value[IsHigh        ] = "3 High"
      Value[IsIntermediate] = "2 Intermediate"
      Value[IsLow         ] = "1 Low"
      #---~---

      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( IsHigh | IsIntermediate | IsLow )
      Value[! IsValid] = NA_character_
      #---~---

   }else if (TraitID %in% c(37L,1251L)){
      #---~---
      #   Leaf phenology type
      #   Plant vegetative phenology (leaf phenology)
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---


      #---~---
      #   Some input variables are ancillary or refer to classification algorithms or
      # information that is too detailed to retain. We discard these values.
      #---~---
      IsTMI = 
         ( ( NameOrig %in% c( "phenology2", "lal link average length"
                            , "leaf retention time (yr)", "growing period"
                            , "active growth period", "growth priod"
                            , "gr; main growth period","seasonality of growth"
                            , "leaf retention time (yr)"
                            )#end c
           )#end NameOrig
         | ( ( AuthorName %in% "Teja Kattenborn" )
           & ( NameOrig %in% "leaf phenology type" )
           )#end Teja Kattenborn
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---


      #---~---
      #   The numeric system points to green peak, but it is not clear whether the plants
      # are deciduous or evergreen.  We assume that those with no clear peak or winter 
      # peak must be evergreen, and the others might be deciduous or semi-deciduous.
      #---~---
      #   Johannes Cornelissen, 4 classes:
      #   1 - no clear peak
      #   2 - winter;autumn;early spring
      #   3 - late spring; spring; spring/summer
      #   4 - late spring/summer; summer
      #---~---
      IsAuthor             = 
         ( ( AuthorName %in% "Johannes Cornelissen" )
         & ( NameOrig   %in% "shoot phenology (seasonality of maximum production of photosynthetic tissue)" )
         )#end IsAuthor
      IsEvergreen          = IsAuthor & ( Value %in% c("1","2") )
      IsSpringGreen        = IsAuthor & ( Value %in% c("3")     )
      IsSummerGreen        = IsAuthor & ( Value %in% c("4")     )
      Value[IsEvergreen  ] = "evergreen"
      Value[IsSpringGreen] = "springgreen"
      Value[IsSummerGreen] = "summergreen"
      #---~---
      #   Sandra Diaz, 5 classes:
      #   1 - no clear peak
      #   2 - winter;autumn;early spring
      #   3 - late spring; spring; spring-summer
      #   4 - late summer-early autumn
      #   5 - late spring-summer; summer
      #---~---
      IsAuthor             = 
         ( AuthorName %in% "Sandra Diaz" ) & ( NameOrig   %in% "shoot phenology" )
      IsEvergreen          = IsAuthor & ( Value %in% c("1","2") )
      IsSpringGreen        = IsAuthor & ( Value %in% c("3")     )
      IsSummerGreen        = IsAuthor & ( Value %in% c("4","5") )
      Value[IsEvergreen  ] = "evergreen"
      Value[IsSpringGreen] = "springgreen"
      Value[IsSummerGreen] = "summergreen"
      #---~---
      #   Marco Moretti, 3 classes:
      #   1 - aestival green
      #   2 - partial evergreen
      #   3 - evergreen
      #---~---
      IsAuthor             = 
         ( AuthorName %in% "Marco Moretti" ) & ( NameOrig   %in% "leaf persistence" )
      IsEvergreen          = IsAuthor & ( Value %in% c("3") )
      IsSemiDecid          = IsAuthor & ( Value %in% c("2")     )
      IsSummerGreen        = IsAuthor & ( Value %in% c("1") )
      Value[IsEvergreen  ] = "evergreen"
      Value[IsSemiDecid  ] = "semi-deciduous"
      Value[IsSummerGreen] = "summergreen"
      #---~---


      #---~---
      #   Additional adjustments to reduce ambiguity.
      #---~---
      #   Steven Jansen, 3 classes with single letter, which would be confusing. Update
      # values to unambiguous classification.
      #---~---
      IsAuthor = ( AuthorName %in% " Steven Jansen" ) & ( NameOrig %in% "phenology" )
      IsEvergreen          = IsAuthor & ( Value %in% c("e") )
      IsSummerGreen        = IsAuthor & ( Value %in% c("w") )
      IsRainGreen          = IsAuthor & ( Value %in% c("d") )
      Value[IsEvergreen  ] = "evergreen"
      Value[IsSummerGreen] = "summergreen"
      Value[IsRainGreen  ] = "raingreen"
      #---~---
      #   Yusuke Onoda, simplify inconsistent classification.
      #---~---
      IsAuthor = 
         ( AuthorName %in% "Yusuke Onoda" ) & ( NameOrig %in% c( "decev", "evedec") )
      IsEvergreen        = IsAuthor & ( Value %in% c("e","evergreen") )
      IsSemiDecid        = IsAuthor & ( Value %in% c("semi-deciduous","ed") )
      IsDeciduous        = IsAuthor & ( Value %in% c("d","deciduous","db") )
      Value[IsEvergreen] = "evergreen"
      Value[IsSemiDecid] = "semi-deciduous"
      Value[IsDeciduous] = "deciduous"
      #--- Walton Green: use leaf retention as proxy for evergreen/deciduous
      IsAuthor    = ( AuthorName %in% "Walton Green" ) & ( NameOrig %in% "leaf retention" )
      IsEvergreen = IsAuthor & ( Value %in% "yes" )
      IsDeciduous = IsAuthor & ( Value %in% "no"  )
      Value[IsEvergreen] = "evergreen"
      Value[IsDeciduous] = "deciduous"
      #--- Marina Scalon: reduce ambiguity and simplify inconsistent classification
      IsAuthor = 
         ( AuthorName %in% "Marina Scalon" ) & ( NameOrig %in% c( "phenology", "type") )
      IsEvergreen         = IsAuthor & ( Value %in% c("evergreen"        ,"ev") )
      IsBreviDecid        = IsAuthor & ( Value %in% c("briefly deciduous","bd") )
      IsDeciduous         = IsAuthor & ( Value %in% c("deciduous"        ,"dc") )
      Value[IsEvergreen ] = "evergreen"
      Value[IsBreviDecid] = "brevi-deciduous"
      Value[IsDeciduous ] = "deciduous"
      #--- Colleen Seymour: they provided leafless time.
      IsAuthor    = ( ( AuthorName %in% "Colleen Seymour" )
                    & ( NameOrig %in% "leaf phenology (months without leaves)" )
                    )#end IsAuthor
      suppressWarnings({
         IsEvergreen  = IsAuthor & ( as.numeric(Value) %le% 0. )
         IsBreviDecid = ( IsAuthor
                        & ( ( as.numeric(Value) %gt% 0. ) & ( as.numeric(Value) %le% 1. ) )
                        )#end IsAuthor
         IsDeciduous  = IsAuthor & ( as.numeric(Value) %gt% 1. )
      })#end suppressWarnings
      Value[IsEvergreen ] = "evergreen"
      Value[IsBreviDecid] = "brevi-deciduous"
      Value[IsDeciduous ] = "deciduous"
      #---~---


      #---~---
      #   Some original names are "yes/no" questions, or variable names that indicate
      # the grow form. We override previous classes.
      #---~---
      IsEvergreen =  ( ( NameOrig %in% c( "leaf phenology: evergreen", "evergreen") )
                     & ( Value    %in% c( "y", "yes" ) )
                     )#end IsEvergreen
      IsBreviDecid = ( ( NameOrig %in% c("leaf phenology: leaf exchanger") )
                     & ( Value %in% c("y","yes") )
                     )#end IsDeciduous
      IsSemiDecid =  ( ( NameOrig %in% "leaf phenology: semi-deciduous" )
                     & ( Value %in% "yes" )
                     )#end IsSemiDecid
      IsDeciduous =  ( ( NameOrig %in% c("leaf phenology: deciduous", "deciduous") )
                     & ( Value %in% c("y","yes") )
                     )#end IsDeciduous
      Value[IsEvergreen ] = "evergreen"
      Value[IsBreviDecid] = "brevi-deciduous"
      Value[IsSemiDecid ] = "semi-deciduous"
      Value[IsDeciduous ] = "deciduous"
      #---~---





      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      Discard        = 
         ( ! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE)
         | ( Value %in% c( "?", "broadleaved", "deciduous or evergreen broad-leaved"
                         , "deciduous or evergreen needle-leaved"
                         , "deciduous or evergreen scale-like", "d_ev"
                         , "http://www.rogerstreesandshrubs.com/gallery/displayblock~bid~11723~gid~~source~gallerydefault.asp"
                         , "megaphanerophyte", "n", "n.d.", "no", "other", "sessile", "x"
                         , "y/n","seasonal biomass peak"
                         )#end c
           )#end Value
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Reduce the number of classes for simplicity
      #---~---
      IsEvergreen   = 
         Value %in% c( "always overwintering green", "always persistent green", "e", "ev"
                     , "evergeen", "evergreen", "evergreen broad-leaved", "evergreen grass"
                     , "evergreen needle-leaved", "evergreen scale-like"
                     , "evergreen  type 1", "evergreen type 2", "evergren", "everggreen"
                     , "http://plants.jstor.org/compilation/blepharispermum.zanguebaricum"
                     , "http://tropical.theferns.info/viewtropical.php?id=borrichia+arborescens"
                     , "http://www.photomazza.com/?bauhinia-integrifolia&lang=en"
                     , "perennifolio", "phyllodium", "w"
                     )#end c
      IsSpringGreen = 
         Value %in% c( "always spring green", "hibernal", "springgreen", "vernal")
      IsSummerGreen = 
         Value %in% c( "always summer green", "aestival", "cold deciduous", "summergreen"
                     , "winter deciduous"
                     )#end c
      IsRainGreen   = 
         Value %in% c( "drought deciduous", "drought-deciduous", "drought semi-deciduous"
                     , "https://de.wikipedia.org/wiki/mopane", "raingreen"
                     )#end c
      IsDeciduous   = 
         Value %in% c( "caducifolio", "d", "dc", "deciduous", "deciduous broad-leaved"
                     , "deciduous needle-leaved", "deciduous/needlelike"
                     , "deciduous scale-like", "deciduous type 1", "deciduous type 2"
                     , "deciduous type 3", "deciduousecideciduousuous", "decioduous"
                     , "decuduous"
                     , "http://plants.jstor.org/stable/10.5555/al.ap.flora.ftea006542"
                     , "http://tropical.theferns.info/viewtropical.php?id=bauhinia+hildebrandtii"
                     , "http://www.efloras.org/florataxon.aspx?flora_id=2&taxon_id=200012056"
                     , "https://en.wikipedia.org/wiki/colutea", "nonevergreen"
                     , "variable deciduous"
                     )#end c
      IsSemiDecid   = 
         Value %in% c( "deciduous/evergreen", "deciduous-semideciduous"
                     , "deciduous or semi-evergreen broad-leaved"
                     , "deciduous/semideciduous", "deciduous/semi-evergreen"
                     , "deciduous to semi-evergreen", "evergreen/deciduous"
                     , "evergreen/semi-deciduous"
                     , "http://tropical.theferns.info/viewtropical.php?id=albizia+pedicellaris"
                     , "http://www.sunshine-seeds.de/indigofera-heterophylla-43327p.html"
                     , "https://en.wikipedia.org/wiki/phygelius_aequalis", "s", "sd", "semi"
                     , "semi-deciduous", "semi-deciduous to evergreen", "semidecicuous"
                     , "semideciduous", "semideciduous/evergreen"
                     , "semideciduoushttps://sites.unicentro.br/wp/manejoflorestal/8099-2/"
                     , "semi-evergreen/deciduous", "semi-evergreen/drought-deciduous"
                     , "winter semi-deciduous"


                     )#end c
      IsBreviDecid  = 
         Value %in% c( "brevi-deciduous", "briefly deciduous", "evergreen/semi-evergreen"
                     , "evergreen/semievergreen", "exchanger", "semi-evergreen"
                     , "semi-evergreen/evergreen", "semi-evergreen/evergren"
                     , "semievergreen"
                     )#end if
      IsCladophylls = Value %in% c( "aphyllous", "cladophylls", "leafless", "stem succulent")
      #---~---


      #---~---
      #   Apply classification.
      #---~---
      Value[IsEvergreen  ] = "Evergreen"
      Value[IsSpringGreen] = "Springgreen"
      Value[IsSummerGreen] = "Summergreen"
      Value[IsRainGreen  ] = "Raingreen"
      Value[IsDeciduous  ] = "Deciduous (not specified)"
      Value[IsSemiDecid  ] = "Semi-deciduous"
      Value[IsBreviDecid ] = "Brevi-deciduous"
      Value[IsCladophylls] = "Cladophylls"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid 
                         & ( IsEvergreen   | IsSpringGreen | IsSummerGreen | IsRainGreen
                           | IsDeciduous   | IsSemiDecid   | IsBreviDecid  | IsCladophylls )
                         )#end Valid
      Value[! IsValid] = NA_character_
      #---~---


   }else if (TraitID %in% c(38L)){
      #---~---
      #   Plant woodiness
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---


      #---~---
      #   Some input variables are ancillary or refer to classification algorithms or
      # information that is too detailed to retain. We discard these values.
      #---~---
      IsTMI = 
        ( ( ( AuthorName %in% "Benjamin Blonder" ) & ( NameOrig %in% "herbaceous" ) ) )
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---


      #---~---
      #   Some authors provided different classification systems that could be ambiguous
      # using a generic assignment. Map their classes onto a unequivocal set of names.
      #---~---
      #   F Stuart Chapin III, 3 numerical classes:
      #   0   - non-woody
      #   0.5 - semi-woody
      #   1   - woody
      #---~---
      IsAuthor             = 
         ( AuthorName %in% "F Stuart Chapin III" ) & ( NameOrig   %in% "woodiness" )
      IsNonWoody         = IsAuthor & ( Value %in% c(  "0") )
      IsSemiWoody        = IsAuthor & ( Value %in% c("0.5") )
      IsWoody            = IsAuthor & ( Value %in% c(  "1") )
      Value[IsNonWoody ] = "non-woody"
      Value[IsSemiWoody] = "semi-woody"
      Value[IsWoody    ] = "woody"
      #---~---
      #   Johannes Cornelissen, two systems due to two data sets but they can be combined
      #   Original Name GrowthForm2
      #      1   - non-woody
      #      h   - non-woody
      #      1.5 - semi-woody
      #      2   - woody
      #      w   - woody
      #---~---
      IsAuthor           = 
         ( ( AuthorName %in% "Johannes Cornelissen"       )
         & ( NameOrig   %in% c("growthform2","woodiness") ) )
      IsNonWoody         = IsAuthor & ( Value %in% c("1","h") )
      IsSemiWoody        = IsAuthor & ( Value %in% c("1.5"  ) )
      IsWoody            = IsAuthor & ( Value %in% c("2","w") )
      Value[IsNonWoody ] = "non-woody"
      Value[IsSemiWoody] = "semi-woody"
      Value[IsWoody    ] = "woody"
      #---~---
      #   Will Cornwell, three systems due to multiple data sets, but they can be combined
      #      h        - non-woody
      #      n        - non-woody
      #      variable - semi-woody
      #      w        - woody
      #      y        - woody
      #---~---
      IsAuthor           = 
         ( ( AuthorName %in% "Will Cornwell" )
         & ( NameOrig   %in% c("gf2","woodiness","woody") ) )
      IsNonWoody         = IsAuthor & ( Value %in% c("h","n"   ) )
      IsSemiWoody        = IsAuthor & ( Value %in% c("variable") )
      IsWoody            = IsAuthor & ( Value %in% c("w","y"   ) )
      Value[IsWoody    ] = "woody"
      Value[IsSemiWoody] = "semi-woody"
      Value[IsNonWoody ] = "non-woody"
      #---~---
      #   Sandra Diaz, numeric classification system
      #      0   - non-woody
      #      1   - semi-woody
      #      2,3 - woody (low/high wood density)
      #---~---
      IsAuthor           = 
         ( AuthorName %in% "Sandra Diaz" ) & ( NameOrig %in% c("woodiness") )
      IsNonWoody         = IsAuthor & ( Value %in% c("0"    ) )
      IsSemiWoody        = IsAuthor & ( Value %in% c("1"    ) )
      IsWoody            = IsAuthor & ( Value %in% c("2","3") )
      Value[IsWoody    ] = "woody"
      Value[IsSemiWoody] = "semi-woody"
      Value[IsNonWoody ] = "non-woody"
      #---~---
      #   Wenxuan Han, two systems due to different data bases (herb/woody and woodiness),
      # but they can assessed together
      #      h        - non-woody
      #      w, woody - woody
      #---~---
      IsAuthor           = 
         ( AuthorName %in% "Wenxuan Han" ) & ( NameOrig %in% c("herb/woody","woody") )
      IsNonWoody         = IsAuthor & ( Value %in% c("h"        ) )
      IsWoody            = IsAuthor & ( Value %in% c("w","woody") )
      Value[IsWoody    ] = "woody"
      Value[IsNonWoody ] = "non-woody"
      #---~---
      #   Sarah Otto, three categories
      #      h        - non-woody
      #      variable - semi-woody
      #      w        - woody
      #---~---
      IsAuthor           = 
         ( ( AuthorName %in% "Sarah Otto"                  )
         & ( NameOrig   %in% c("woodiness (w; h; variable)") ) )
      IsNonWoody         = IsAuthor & ( Value %in% c("h"       ) )
      IsSemiWoody        = IsAuthor & ( Value %in% c("variable") )
      IsWoody            = IsAuthor & ( Value %in% c("w"       ) )
      Value[IsWoody    ] = "woody"
      Value[IsSemiWoody] = "semi-woody"
      Value[IsNonWoody ] = "non-woody"
      #---~---
      #   Peter Reich, herb/grass classification:
      #      h   - non-woody
      #      w   - woody
      #---~---
      IsAuthor           = 
         ( AuthorName %in% "Peter Reich" ) & ( NameOrig   %in% "woodyherb" )
      IsNonWoody         = IsAuthor & ( Value %in% c("h") )
      IsWoody            = IsAuthor & ( Value %in% c("w") )
      Value[IsWoody    ] = "woody"
      Value[IsNonWoody ] = "non-woody"
      #---~---


      #---~---
      #   Reduce the number of classes for simplicity
      #---~---
      IsWoody     =
         Value %in% c( "liana", "wood at base", "woody", "woody at base", "woody base" )
      IsSemiWoody =
         Value %in% c( "fibrous", "non-woody/woody", "semi-woody", "suffrutex", "variable"
                     , "woody/non-woody", "woody/nonwoody", "woody rootstock"
                     )#end c
      IsNonWoody  =
         Value %in% c( "grass&sedges", "hearbaceous", "herb", "herb; graminoid"
                     , "herbaceous", "herbaceous epiphyte", "non woody", "non-woody"
                     , "nono-woody"
                     )#end c
      #---~---


      #---~---
      #   Apply classification.
      #---~---
      Value[IsWoody    ] = "Woody"
      Value[IsSemiWoody] = "Semi-woody"
      Value[IsNonWoody ] = "Non-woody"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid  & ( IsWoody | IsSemiWoody | IsNonWoody ) )
      Value[! IsValid] = NA_character_
      #---~---


   }else if (TraitID %in% c(42L)){
      #---~---
      #   Plant growth form
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---


      #---~---
      #   Some authors provided classification that is too generic for the growth form
      # classification.
      #---~---
      IsAuthor        = AuthorName %in% c( "Adam Martin", "Fritz Schweingruber"
                                         , "Sabina Burrascano"
                                         )#end c
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---


      #---~---
      #   Some authors provided multiple classifications. Retain only one of them.
      #---~---
      IsAuthor        = ( ( AuthorName %in% c( "William Cornwell") )
                        & ( NameOrig   %in% c( "growth form (consensus)") )
                        )#end IsAuthor
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---


      #---~---
      #   Some authors used very short names making it difficult to discern the meaning
      # of the classes provided.  But because the original variable is short too, we 
      # use a combination of author and name to make the data.
      #---~---
      #--- Classes and original reference are in Russian, skipping for now.
      IsTMI        = ( ( AuthorName %in% "Serge Sheremetev" ) 
                     & ( NameOrig %in% c("ggf","sgf") )
                     )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Original name is common but not informative for specific authors
      IsTMI        = ( ( AuthorName %in% "Tim Flowers" ) 
                     & ( NameOrig %in% c("plant type") )
                     )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- 
      IsTMI        = ( ( AuthorName %in% "Walton Green" ) 
                     & ( NameOrig %in% c( "growth form", "low growing grass"
                                        , "shape and orientation"
                                        )#end c
                       )#end NameOrig
                     )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Classes would be confusing due to c meaning either cactus or climber.
      Value[( AuthorName %in% "Steven Jansen"   ) & ( Value %in% "c"           )] = "cactus"
      Value[( AuthorName %in% "Steven Jansen"   ) & ( Value %in% "f"           )] = "fern"
      Value[( AuthorName %in% "Steven Jansen"   ) & ( Value %in% "g"           )] = "grass"
      Value[( AuthorName %in% "Steven Jansen"   ) & ( Value %in% "h"           )] = "herb"
      Value[( AuthorName %in% "Steven Jansen"   ) & ( Value %in% "l"           )] = "liana"
      Value[( AuthorName %in% "Steven Jansen"   ) & ( Value %in% "s"           )] = "shrub"
      Value[( AuthorName %in% "Steven Jansen"   ) & ( Value %in% "t"           )] = "tree"
      #--- Classes would be confusing due to c meaning either cactus or climber.
      Value[( AuthorName %in% "Will Cornwell"   ) & ( Value %in% "c"           )] = "cactus"
      Value[( AuthorName %in% "Will Cornwell"   ) & ( Value %in% "g"           )] = "grass"
      Value[( AuthorName %in% "Will Cornwell"   ) & ( Value %in% "h"           )] = "herb"
      Value[( AuthorName %in% "Will Cornwell"   ) & ( Value %in% "s"           )] = "shrub"
      Value[( AuthorName %in% "Will Cornwell"   ) & ( Value %in% "t"           )] = "tree"
      Value[( AuthorName %in% "Will Cornwell"   ) & ( Value %in% "v"           )] = "vine"
      #--- Classes somewhat confusing as "w" means tree in this context.
      Value[( AuthorName %in% "Yusuke Onoda"    ) & ( Value %in% "g"           )] = "grass"
      Value[( AuthorName %in% "Yusuke Onoda"    ) & ( Value %in% "h"           )] = "herb"
      Value[( AuthorName %in% "Yusuke Onoda"    ) & ( Value %in% c("w","woody"))] = "tree"
      #--- Ensure that entries by Chris Baraloto are correctly assigned.
      Value[( AuthorName %in% "Chris Baraloto"  ) & ( Value %in% "p"           )] = "palm"
      Value[( AuthorName %in% "Chris Baraloto"  ) & ( Value %in% c("l","liana"))] = "liana"
      Value[( AuthorName %in% "Chris Baraloto"  ) & ( Value %in% "a"           )] = "tree"
      #---~---




      #---~---
      #   Data base uses "woody" instead of "tree". Switch it to remove ambiguity.
      #---~---
      IsAuthor      = ( ( AuthorName %in% "Gregoire Freschet" )
                      & ( NameOrig   %in% "plant type"        ) )
      IsTree        = IsAuthor & ( Value %in% c("woody deciduous","woody evergreen") )
      Value[IsTree] = "tree"
      #---~---


      #---~---
      #   Data base uses "woody" instead of "tree". Switch it to remove ambiguity.
      #---~---
      IsAuthor      = ( AuthorName %in% "Peter Reich" ) & ( NameOrig %in% "gymnangioth" )
      IsTree        = IsAuthor & ( Value %in% c("angwood","gymn") )
      IsHerb        = IsAuthor & ( Value %in% c("angherb")        )
      Value[IsTree] = "tree"
      Value[IsHerb] = "herb"
      #---~---


      #---~---
      #   Data base uses "free-standing" instead of "tree". Switch it to remove ambiguity.
      #---~---
      IsAuthor          = ( ( AuthorName %in% "William Cornwell" ) 
                          & ( NameOrig %in% "growth form complete" ) )
      IsTree            = IsAuthor & ( Value %in% c("free-standing") )
      IsParasite        = IsAuthor & ( Value %in% c("holo-mycoheterotroph") )
      Value[IsTree]     = "tree"
      Value[IsParasite] = "parasite"
      #---~---


      #---~---
      #   Data base sampled mostly forbs and grasses. Replace legumes with herbs so the
      # information is not lost.
      #---~---
      IsAuthor      = ( ( AuthorName %in% "Justin Luong" ) 
                      & ( NameOrig %in% "plant growth form" ) )
      IsHerb        = IsAuthor & ( Value %in% c("annual legume", "perennial legume") )
      Value[IsHerb] = "herb"
      #---~---


      #---~---
      #   Simplify and standardise name in the data base.
      #---~---
      IsAuthor      = 
         ( ( AuthorName %in% "Serge Sheremetev" ) 
         & ( NameOrig %in% c("combined","ecological type","lf") ) )
      IsAquatic     = IsAuthor & ( Value %in% c("a","ha","hydrophyte") )
      IsEpiphyte    = IsAuthor & ( Value %in% c("epiphyte (e)") )
      IsHerb        = IsAuthor & ( Value %in% c("h","herb (h)") )
      IsLiana       = IsAuthor & ( Value %in% c("l","liana (l)") )
      IsPalm        = IsAuthor & ( Value %in% c("p","palm (p)") )
      IsShrub       = IsAuthor & ( Value %in% c("s","shrub (s)") )
      IsTree        = IsAuthor & ( Value %in% c("t","tree (t)") )
      IsXerophyte   = 
         ( IsAuthor 
         & ( Value %in% c( "c", "cacti (c)", "succulent", "succulent with kranz anatomy"
                         , "xerophyte"
                         )#end c
           )#end Value
         )#end IsAuthor
      Discard       = 
         ( IsAuthor
         & ( Value %in% c( "epiphyte; liana (el)", "herb; liana (hl)", "herb; shrub (hs)"
                         , "hl", "hs", "hsa", "hsl", "hslt", "hst", "hygrophyte"
                         , "liana shrub; tree (lst)", "liana; shrub (ls)", "lt"
                         , "mesophyte", "mesoxerophyte","sa","shrub; treee (st)"
                         , "sl", "slt", "st", "tree; liana (tl)", "xeromesophyte"
                         )#end c
           )#end Value
         )#end IsAuthor
      Value[IsAquatic  ] = "aquatic"
      Value[IsEpiphyte ] = "epiphyte"
      Value[IsHerb     ] = "herb"
      Value[IsLiana    ] = "liana"
      Value[IsPalm     ] = "palm"
      Value[IsShrub    ] = "shrub"
      Value[IsTree     ] = "tree"
      Value[IsXerophyte] = "xerophyte"
      Value[Discard    ] = NA_character_
      Valid[Discard    ] = FALSE
      VName[Discard    ] = NA_character_
      #---~---


      #---~---
      #   Some original names are "yes/no" questions, or variable names that indicate
      # the grow form.
      #---~---
      IsAquatic     = 
         ( ( NameOrig %in% "aquatic" )
         | ( ( NameOrig %in% "growth form additional info" )
           & ( Value %in% c( "aquatic fresh water", "aquatic salt water"
                           , "aquatic; fresh water; floating"
                           )#end c
             )#end Value
           )#end NameOrig
         )#end IsAquatic
      IsCarnivorous = 
         ( ( NameOrig %in% c("carnivorous","carnivory") ) & ( Value %in% "yes" ) )
      IsEpiphyte    = 
         ( ( ( NameOrig %in% "epiphytic"                   ) & ( Value %in% "yes"       ) )
         | ( ( NameOrig %in% "habit"                       ) & ( Value %in% "epiphytic" ) )
         | ( ( NameOrig %in% "growth form additional info" ) & ( Value %in% "epiphyte"  ) )
         )#end IsEpiphyte
      IsFern        = 
         ( ( NameOrig %in%   "life form: fern/fern ally" )   & ( Value %in% "yes" ) )
      IsGrass       = 
         ( ( ( NameOrig %in% "crop" ) & grepl(pattern="^http",x=Value) )
         | ( ( NameOrig %in% c( "crop", "forb", "gram", "graminoid", "graminoids"
                            , "life form: forb", "life form: geophyte"
                            , "life form: graminoid"
                            )#end c 
             )#end NameOrig
           & ( Value %in% "yes" )
           )#end NameOrig2
         )#end IsGrass
      IsLiana       = 
         ( ( NameOrig %in% "life form: liana" ) & ( Value %in% "yes" ) )
      IsParasite    = 
         ( ( NameOrig %in% "parasite" ) & ( Value %in% "yes" ) )
      IsShrub       = 
         ( ( ( NameOrig %in% c( "prostrate/decumbent", "shrub"
                              , "life form: erect dwarf shrub"
                              , "life form: prostrate dwarf shrub", "life form: shrub"
                              )#end c 
             )#end NameOrig
           & ( Value %in% "yes" )
           )#end prostrate/decumbent
         )#end IsShrub
      IsTree        = 
         ( ( ( NameOrig %in% c( "tree", "life form: small tree", "life form: tree" ) )
           & ( Value %in% "yes" ) 
           )#end NameOrig
         )#end IsTree
      IsVine        = 
         ( ( ( NameOrig %in% "climber" ) & ( ! Value %in% "liana" ) )
         | ( ( NameOrig %in% "life form: climber") & ( Value %in% "yes" ) )
         )#end IsVine
      IsXerophyte   = 
         ( ( ( NameOrig %in% "succulent" ) & (! Value %in% "not succulent" ) )
         | ( ( NameOrig %in% "succulence of leaves or stem" ) & ( Value %in% "presence" ) )
         | ( ( NameOrig %in% c( "succulent species", "leaf succulence"
                              , "stem succulent", "life form: succulent"
                              )#end c
             )#end NameOrig
           & ( Value %in% "yes" ) 
           )#end Succulent species
         | ( ( NameOrig %in% "growth form additional info" ) & ( Value %in% "succulent" ) )
         | ( ( NameOrig %in% "succulence_yes") & ( Value %in% "yes" ) )
         )#end IsXerophyte
      Value[IsAquatic     ] = "aquatic"
      Value[IsCarnivorous ] = "carnivorous"
      Value[IsEpiphyte    ] = "epiphyte"
      Value[IsFern        ] = "fern"
      Value[IsGrass       ] = "grass"
      Value[IsLiana       ] = "liana"
      Value[IsParasite    ] = "parasite"
      Value[IsShrub       ] = "shrub"
      Value[IsTree        ] = "tree"
      Value[IsVine        ] = "vine"
      Value[IsXerophyte   ] = "xerophyte"
      #---~---


      #---~---
      #   Some input variables are ancillary or refer to classification algorithms or
      # information that is too detailed to retain. We discard these values.
      #---~---
      IsTMI = 
         NameOrig %in% c( "amphibious", "climber_yes", "climbingmode", "conifers"
                        , "consensus", "growthform_div", "growthform_org", "leaf suculen"
                        , "life form combined", "life form: epiphyte/parasite"
                        , "multistemness", "multistemness/ growth form"
                        , "myco_heterotrophs", "pft", "plant form: caespitose"
                        , "plant form: climbing", "plant form: cushion"
                        , "plant form: non-distinctive", "plant form: open"
                        , "plant form: prostrate", "plant form: tangled"
                        , "plant form: vase", "plant growth form 2"
                        , "plantgrowthformadditionalattribute", "plant_taxonomy"
                        , "shoot growth form", "species.type", "stem base"
                        , "succulence index", "tda", "tdr", "terrestrial"
                        )#end c
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---



      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      Discard        = 
         ( (! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE) )
         | ( Value %in% c( "absence", "angiosperm", "climber/succulent", "conifers"
                         , "cushion forming", "deciduous shrub or tree"
                         , "elongated; leaf-bearing rhizomatous"
                         , "epiphyte; hemiepiphyte", "epiphyte/succulent"
                         , "evergreen shrub or tree", "foliose lichen"
                         , "forb/herb; shrub; subshrub", "forb/herb; subshrub"
                         , "fruticose lichen", "gymnangioth", "gymnosperm"
                         , "herb|herb|shrub", "herb|herbaceous vine"
                         , "herb|liana/woody vine", "herb/shrub", "herb|shrub"
                         , "herb|shrub|herb", "herb|shrub|herbaceous vine"
                         , "herb|shrub|herbaceous vine|liana/woody vine", "herb|shrub|shrub"
                         , "herb|shrub|tree", "herb/shrub/tree", "herb/tree"
                         , "herbaceous or shrubby", "herbaceous vine|herb"
                         , "herbaceous vine|liana/woody vine", "herbaceous vine|shrub"
                         , "hygrophyte", "legume", "legumes", "liana/woody vine|herb"
                         , "liana/woody vine|shrub", "lichen", "mallee", "mesophyte"
                         , "mesoxerophyte", "monocot", "mycoheterotrophic", "nd", "no"
                         , "non-succulent", "nontree", "not succulent", "sapling"
                         , "saplings", "seedlings", "semi deciduous tree or shrub"
                         , "shrub / tree", "shrub | tree", "shrub/climber", "shrub|herb"
                         , "shrub|herbaceous vine", "shrub|herbaceous vine|liana/woody vine"
                         , "shrub/liana", "shrub|liana/woody vine"
                         , "subshrub; shrub; forb/herb", "subshrub; shrub; graminoid"
                         , "subshrub; shrub; tree", "shrub|shrub|tree", "shrub|tree"
                         , "shrub/tree", "shrub/tree/climber", "shrub/tree intermediate"
                         , "shrub/tree/liana", "shrub/vine", "shrub; tree", "shrub; vine"
                         , "terrestrial", "tree | shrub", "tree shrub intermediate"
                         , "tree/large shrub", "tree/liana", "tree|liana/woody vine"
                         , "tree|shrub", "tree/shrub", "tree-shrub", "tree/shrub/climber"
                         , "tree/shrub/climber/herb", "tree|shrub|shrub", "tree|shrub|tree"
                         , "tree|shrub|tree|shrub", "tree; shrub", "tree; shrub; subshrub"
                         , "tree; shrub; vine", "tree; subshrub; shrub", "t/s"
                         , "unclasified", "unspecified", "various", "vine/shrub"
                         , "vine; shrub", "vine; shrub; graminoid", "vine; subshrub"
                         , "vine | tree", "woody", "woody legume", "woody monocot"
                         , "woody plant", "woody plants", "woody species", "xeromesophyte"
                         )#end c
           )#end Value
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---




      #---~---
      #   Organise results in high-level classes only.
      #---~---
      IsAquatic     = 
         Value %in% c( "amphibiousubmerged", "aquatic", "aquatic forb", "aquatic/fern"
                     , "aquatic_herb", "aquatic/parasitic", "aquatic/succulent"
                     , "graminoid/aquatic", "hydrophyte", "hydrophytes", "seagrass"
                     , "submerged"
                     )#end c
      IsBamboo      = Value %in% c( "bamboo", "culm", "horsetail")
      IsCarnivorous = Value %in% c( "carnivore", "carnivorous")
      IsEpiphyte    = 
         Value %in% c( "climber/epiphyte", "climber/epiphyte/succulent", "ep", "epiphyte"
                     , "epiphyte (mistletoe)", "epiphytes", "mistletoe"
                     )#end c
      IsFern        = 
         Value %in% c( "climber/fern", "epiphyte/fern", "f", "fern", "fern ally"
                     , "fern or fern ally", "fernally", "ferns"
                     , "ferns and allies (lycophytes)", "graminoid/fern", "pteridophyte"
                     , "tree fern", "t (treefern)"
                     )#end c
      IsGrass       = 
         Value %in% c( "annual", "annuals", "annual forb", "annual graminoid"
                     , "annual grass", "annual herb", "bunchgrasses", "c3 grass"
                     , "c4 grass", "c3.sedges", "cereal", "crop", "crops"
                     , "extensive-stemmed herb", "forage grass", "forb"
                     , "forb (herbaceous; with or without woody base)", "forb/herb"
                     , "forbs", "frobs", "g", "geop", "geophyte", "gram", "graminoid"
                     , "graminoids", "graminoids tussock", "gras", "grass", "grass (clonal)"
                     , "grass (poaceae only)", "grass (tussock)", "grasses&sedges"
                     , "grasslike", "h", "hemicryptophyte", "herb", "herb; graminoid"
                     , "herb.", "herb/sub-shrub", "herbaceous", "herbaceous annual"
                     , "herbaceous annual-biennial", "herbaceous dicot"
                     , "herbaceous dicots", "herbaceous forb", "herbaceous legume"
                     , "herbaceous monocot", "herbaceous perennial", "herbaceous plant"
                     , "herbs", "leguminous forb", "lycopodiophyta", "pasture grass"
                     , "perennial forb", "perennial graminoid", "perennial grass"
                     , "perennial grass/hemicryptophyte", "perennial herb"
                     , "perennial herb/hemicryptophyte", "perennial leguminous herb"
                     , "prairie grass", "rhizome grass", "rus", "se", "seges", "sedge"
                     , "sub shrub and herb", "subshrub; forb/herb", "variable forb"
                     , "variable graminoid", "variable grass", "weed", "weed; sedge"
                     )#end c
      IsHemiepiphyte = 
         Value %in% c( "hemi-epipjyte", "hemiepiphite", "hemiepiphyte", "hemiepiphytes"
                     , "hemi-epiphyte"
                     )#end c
      IsLiana        = 
         Value %in% c( "l", "lian", "liana", "liana - top", "liana/woody vine"
                     , "lianas (wody climbers)", "lianna", "tree (evergreen) liana"
                     , "woody bine"
                     )#end c
      IsMoss         = 
         Value %in% c( "bryophyte", "club moss", "clubmoss", "cushion plant", "cushion"
                     , "herb/moss", "mat forming", "mat-forming", "moss", "selaginella"
                     , "turf moss"
                     )#end c
      IsPalm         = 
         Value %in% c( "p", "palm", "palm/tree", "palmoid", "tree/palmtree"
                     , "understory palm", "canopy_palm", "understory_palm"
                     )#end c
      IsParasite     = 
         Value %in% c( "climber/epiphyte/parasitic", "climber/parasitic"
                     , "epiphyte/parasitic", "hemi-parasite", "hemiparasite"
                     , "hemiparasitic", "holoparasitic", "parasitic", "parasite"
                     , "root parasite", "stem parasite"
                     )#end c
      IsShrub        = 
         Value %in% c( "arborescent shrubs", "arbusto", "(shrub)", "chaemaephyte"
                     , "chaemaephyte | nano-chamaephyte", "chaemaephyte | shrub"
                     , "dwarf semishrub", "drwarf shrub", "dwarf shrub", "erect dwarf shrub"
                     , "erect dwarf shub", "evergreen dwarf shrub", "large shrub"
                     , "low to high shrub", "nano-chamaephyte", "prostrate dwarf shrub"
                     , "ps", "s", "scrub", "sh", "shru", "shrub", "shrub (chamaephyte)"
                     , "shrub (woody 1-4m)", "shrub | chaemaephyte"
                     , "shrub | nano-chamaephyte", "shrub; subshrub", "shrub or chamaephyt"
                     , "shrub seedling", "shrubs", "shrubs and sub-shrubs", "shurb", "srub"
                     , "ss", "sub-shrub", "sub-shrub (chamaephyte)", "subshrub", "subshurb"
                     , "subshrub/shrub", "subshrub; shrub", "subshrub (woody <1m)"
                     , "suffrutescent", "woody shrub"
                     )#end c
      IsTree         = 
         Value %in% c( "a", "arbol", "canopy tree", "macrophyte", "mid canopy"
                     , "mid.canopy.tree", "savanna", "small tree", "smtree", "t"
                     , "top canopy", "top.canopy.tree", "tre", "tree", "tree crop"
                     , "tree (woody >4m)", "tree (deciduous)", "tree (evergreen)"
                     , "tree-like", "tree | tree", "trees", "understory", "understory tree"
                     , "canopy_tree", "midtree", "treelet"
                     )#end c
      IsVine         = 
         Value %in% c( "c", "c+sc", "chaemaephyte | vine", "climber", "climber or creeper"
                     , "forb/herb; vine", "herbaceous vine", "sc", "twiner/climber.", "v"
                     , "vine", "vine; forb/herb", "vine or liana"
                     , "vines (non-woody climbers)"
                     )#end c
      IsXerophyte    = 
         Value %in% c( "cact", "cactus", "desert sub-shrubs", "rosette", "rosette forb"
                     , "rosette plant", "succulent", "succulent | annual"
                     , "succulent | tree", "succulent leaves", "succulent stems"
                     , "succulent with kranz anatomy", "xerophyte"
                     )#end c
      #---~---


      #---~---
      #   Assign data to one of the existing classes.
      #---~---
      Value[IsAquatic     ] = "Aquatic"
      Value[IsBamboo      ] = "Bamboo"
      Value[IsCarnivorous ] = "Carnivorous"
      Value[IsEpiphyte    ] = "Epiphyte"
      Value[IsFern        ] = "Fern"
      Value[IsGrass       ] = "Grass-Herb"
      Value[IsHemiepiphyte] = "Hemiepiphyte"
      Value[IsLiana       ] = "Liana"
      Value[IsMoss        ] = "Moss"
      Value[IsPalm        ] = "Palm"
      Value[IsParasite    ] = "Parasite"
      Value[IsShrub       ] = "Shrub"
      Value[IsTree        ] = "Tree"
      Value[IsVine        ] = "Vine"
      Value[IsXerophyte   ] = "Xerophyte"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( IsAquatic      | IsBamboo       | IsCarnivorous 
                           | IsEpiphyte     | IsFern         | IsGrass
                           | IsHemiepiphyte | IsLiana        | IsMoss
                           | IsPalm         | IsParasite     | IsShrub
                           | IsTree         | IsVine         | IsXerophyte
                           )#end Valid
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---


   }else if (TraitID %in% c(95L)){
      #---~---
      #   Seed germination rate (germination efficiency)
      #---~---

      #--- Make sure all data have the same units (%).
      fr_sel        = UnitOrig %in% c("dimensionless","ratio")
      pc_sel        = UnitOrig %in% c("%","percent")
      Value[fr_sel] = as.character(frac2pc * as.numeric(Value[fr_sel]))
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( fr_sel | pc_sel )
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---

   }else if (TraitID %in% c(146L,151L,167L)){
      #---~---
      #   Leaf carbon/nitrogen (C/N) ratio
      #   Leaf carbon/phosphorus (C/P) ratio
      #   Fine root carbon/nitrogen (C/N) ratio
      #---~---




      #--- Make sure all data have the same units (kgC/kgN or kgC/kgP).
      kgCokgX_sel        = 
         UnitOrig %in% c("% / %","%/%","unitless","ratio","g/g","Ratio","mg/mg","%C/%N"
                        , "kg C per kg N","g*g-1")
      kgXokgC_sel        = UnitOrig %in% c("g(N)/g(C)")
      bad_sel            = UnitOrig %in% c("text")
      Value[kgCokgX_sel] = as.character(1. * as.numeric(Value[kgCokgX_sel]))
      Value[kgXokgC_sel] = as.character(1. / as.numeric(Value[kgXokgC_sel]))
      Value[bad_sel    ] = NA_character_
      Valid[bad_sel    ] = FALSE
      VName[bad_sel    ] = NA_character_
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( kgCokgX_sel | kgXokgC_sel )
      Value[! IsValid] = NA_character_
      #---~---

   }else if(TraitID %in% c(171L)){
      #---~---
      #   Stem sapwood cross-sectional area per supported leaf surface area (Huber value)
      #---~---


      #---~---
      #   Discard data that are not based at the breast height.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Maurizio Mencuccini" )
                 & ( NameOrig   %in% "values at base of living crown; m2/cm2" )
                 )#end IsAuthor
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---



      #---~---
      #   Relying on units only can be misleading in this case. We check author and
      # original variable names.
      #---~---
      m2lom2s_sel  = ( ( AuthorName %in% "Will Cornwell") 
                     & ( NameOrig   %in% "leaf.area.per.sapwood.area" )
                     )#end m2lom2s_sel
      m2locm2s_sel = ( ( ( AuthorName %in% "Pengcheng He" )
                       & ( NameOrig   %in% "the ratio of leaf area attached per unit sapwood cross-section area (m2 cm-2)" )
                       )#end Pengcheng He
                     | ( ( AuthorName %in% "Maurizio Mencuccini" )
                       & ( NameOrig   %in% "values at breast height; m2/cm2" )
                       )#end Maurizio Mencuccini
                     )#end m2locm2s_sel
      m2som2l_sel  = ( AuthorName %in% "Steven Jansen") & ( NameOrig %in% "huber value" )
      cm2som2l_sel = ( ( UnitOrig %in% c("m2sapwood m-2leaf x 10-4") )
                     | ( ( NameOrig %in% "huber value: hv" ) & ( UnitOrig %in% "cm2 m-2" ) )
                     )#end cm2som2l_sel
      #---~---


      #--- Make sure all data have the same units (cm2sapwood/m2leaf).
      Value[m2lom2s_sel ] = as.character(m2.2.cm2 / as.numeric(Value[m2lom2s_sel ]))
      Value[m2locm2s_sel] = as.character(1.       / as.numeric(Value[m2locm2s_sel]))
      Value[m2som2l_sel ] = as.character(m2.2.cm2 * as.numeric(Value[m2som2l_sel ]))
      Value[cm2som2l_sel] = as.character(1.       * as.numeric(Value[cm2som2l_sel]))
      #---~---


      #---~---
      #   Unset value in case some case has not been accounted for (but keep it valid
      # so the code crashes and people are aware that they need to update this block).
      #---~---
      IsValid = Valid & ( m2lom2s_sel | m2locm2s_sel | m2som2l_sel | cm2som2l_sel )
      Value[! IsValid] = NA_character_
      #---~---

   }else if(TraitID %in% c(188L,189L,3468L)){
      #---~---
      #   Leaf water & osmotic potential: leaf osmotic potential at full turgor
      #   Leaf water & osmotic potential: leaf osmotic potential at turgor loss
      #   Leaf water & osmotic potential: leaf water potential at turgor loss point
      #---~---

      #--- Make sure all data have the same units (MPa).
      MPa_sel          = UnitOrig %in% c("Mpa","MPa","-Mpa","-MPa")
      Value[MPa_sel]  = as.character(-1.            * abs(as.numeric(Value[MPa_sel])))
      #---~---


      #---~---
      #   Unset value in case some case has not been accounted for (but keep it valid
      # so the code crashes and people are aware that they need to update this block).
      #---~---
      IsValid          = Valid & ( MPa_sel )
      Value[! IsValid] = NA_character_
      #---~---


   }else if(TraitID %in% c(190L)){
      #---~---
      #   Leaf elastic modulus
      #---~---

      #--- Make sure all data have the same units (MPa).
      MPa_sel          = UnitOrig %in% c("Mpa","MPa")
      #---~---


      #---~---
      #   Unset value in case some case has not been accounted for (but keep it valid
      # so the code crashes and people are aware that they need to update this block).
      #---~---
      IsValid          = Valid & ( MPa_sel )
      Value[! IsValid] = NA_character_
      #---~---



   }else if(TraitID %in% c(191L,710L)){
      #---~---
      #   Leaf water content apoplastic
      #   Leaf water content at turgor loss point
      #---~---

      #--- Make sure all data have the same units (%).
      fr_sel        = UnitOrig %in% c("dimensionless","g/g")
      pc_sel        = UnitOrig %in% c("%","percent","g/100g")
      Value[fr_sel] = as.character(frac2pc * (as.numeric(Value[fr_sel])))
      #---~---


      #---~---
      #   Unset value in case some case has not been accounted for (but keep it valid
      # so the code crashes and people are aware that they need to update this block).
      #---~---
      IsValid          = Valid & ( fr_sel | pc_sel )
      Value[! IsValid] = NA_character_
      #---~---


   }else if(TraitID %in% c(197L)){
      #---~---
      #   Plant functional type (PFT)
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value = tolower(Value)
      #---~---


      #---~---
      #   Some authors used multiple PFT classification systems for the same observation.
      # We keep only one of them for the record to avoid redundancies or inconsistencies.
      #---~---
      IsTMI = 
         ( ( AuthorName %in% "Owen Atkin" )
         & ( NameOrig   %in% c("pft_exeter","pft_jules","pft_jules2","pft_lpj"
                              ,"pft_sheffield")                                  )
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      IsTMI = 
         ( ( AuthorName %in% "Ian Wright" )
         & ( NameOrig   %in% c("bgc1", "bgc2", "sheff1", "sheff2", "sheff3") )
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---




      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      IsBad          =
         Value %in% c( "arctic", "climax", "early secondary", "forb", "grass"
                     , "late secondary", "legume", "maturephase tree", "na"
                     , "pioneer shrub", "pioneer tree", "understorey shrub"
                     , "understorey tree", "vine", "xero-mesophyte", "xerophyte"
                     )#end c
      Discard        = ( ! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE)
                       | IsBad
                       )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_   
      #---~---


      #---~---
      #   Reduce the number of classes for simplicity. Because many authors did not
      # distinguish broadleaf trees between tropical, temperate, boreal/austral, we
      # do not assign biomes to them, instead simply assign evergreen and deciduous
      #---~---
      IsBLDeTree = Value %in% c( "andcblt", "bdt_te", "bldctmp", "bldctrp", "bordcbl"
                               , "boreal deciduous", "broadleaved deciduous"
                               , "c3_broadleaf_deciduous trees", "da", "dcblt"
                               , "deciduous broadleaf", "gydcblt", "savanna"
                               , "savanna deciduous", "tdb", "temperate deciduous"
                               , "tmpdcbl", "trpdcbl"
                               )#end c
      IsBLEvTree = Value %in% c( "anevblt", "bet_te", "bet_tr", "blevtmp", "blevtrp"
                               , "borevbl", "broadleaved evergreen", "ea", "eg"
                               , "evergreen broadleaf", "evblt", "gyevblt", "rainforest"
                               , "savanna evergreen", "semi-arid evergreen","seb"
                               , "teb", "temperate evergreen", "temperate evergreen legume"
                               , "tmpevbl", "trpevbl"
                               )#end c
      IsNLDeTree = Value %in% c( "bordcnl", "dg", "dcnlt", "gydcnlt", "tmpevnl", "tdn"
                               )#end c
      IsNLEvTree = Value %in% c( "boreal conifer", "borevnl", "c3_needle_evergreen trees"
                               , "net_b", "net_te", "nlev", "sub-alpine conifer"
                               , "temperate conifer", "tmpevnl", "gyevnlt","evnlt", "ten"
                               )#end c
      IsC3Grass  = Value %in% c( "c3 grass", "c3 grassland", "crop c3", "esophyte", "gc3"
                               , "mountain grassland", "c3h", "herb; c3; agricultural"
                               , "herb; ag", "herb; agricultural", "anc3h", "ptc3h"
                               )#end c
      IsC4Grass  = Value %in% c( "c4 grass", "c4 grassland", "gc4", "c4h"
                               , "herb; c4; agricultural", "anc4h"
                               )#end c
      IsDeShrub  = Value %in% c( "arctic tundra", "c3 shrub", "andcbls", "dcs"
                               )#end c
      IsEvShrub  = Value %in% c( "desert shrub", "sev", "anevbls", "anevblsc4", "evs"
                               , "gyevbls", "evsh"
                               )#end c
      #---~---


      #---~---
      #   Apply classification. For now we lump together 
      # all the deciduous types into one category, because most entries do not distinguish
      # them. This can be revised in the future.
      #---~---
      Value[IsBLDeTree] = "Broadleaf Deciduous Tree"
      Value[IsBLEvTree] = "Broadleaf Evergreen Tree"
      Value[IsNLDeTree] = "Needleleaf Deciduous Tree"
      Value[IsNLEvTree] = "Needleleaf Evergreen Tree"
      Value[IsC3Grass ] = "C3 Grass"
      Value[IsC4Grass ] = "C4 Grass"
      Value[IsDeShrub ] = "Deciduous Shrub"
      Value[IsEvShrub ] = "Evergreen Shrub"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( IsBLDeTree | IsBLEvTree | IsNLDeTree | IsNLEvTree
                                 | IsC3Grass  | IsC4Grass  | IsDeShrub  | IsEvShrub  )
      Value[! IsValid] = NA_character_
      #---~---



   }else if(TraitID %in% c(335L)){
      #---~---
      #   Plant reproductive phenology timing
      #---~---

      #---~---
      #   Try to convert the values to integer.
      #---~---
      suppressWarnings({
         ValueINT = as.integer(round(as.numeric(Value)))
         int_fine = is.finite(ValueINT)
      })#end suppressWarnings
      #---~---

      #---~---
      #   Retrieve units.
      #---~---
      unit_doy   = UnitOrig %in% c("(Julian day)","Julian Day","day","days"
                                  ,"day of year","doy")
      unit_mon   = UnitOrig %in% c("mon","month")
      unit_miss  = is.na(UnitOrig)
      #---~---


      #---~---
      #   Some phenology data have zeroes.  It is unclear what these are supposed to
      # mean so we discard them.
      #---~---
      zero_sel            = (ValueINT %eq% 0L) & (unit_doy | unit_mon)
      Value    [zero_sel] = NA_character_
      ValueINT [zero_sel] = NA_integer_
      unit_doy [zero_sel] = FALSE
      unit_mon [zero_sel] = FALSE
      unit_miss[zero_sel] = TRUE
      int_fine [zero_sel] = FALSE
      Valid    [zero_sel] = FALSE
      VName    [zero_sel] = NA_character_
      #---~---




      #---~---
      #   First guess for entries.
      #---~---
      mon_likely = ( unit_mon | unit_miss ) & all( ValueINT[int_fine] %in% sequence(12L))
      doy_likely = ( ( ! mon_likely ) & ( unit_doy | unit_miss | unit_mon )
                   & all( ValueINT[int_fine] %in% sequence(366L) )
                   )#end doy_likely
      #---~---

      #---~---
      #   Make sure all data have the same units (month).
      #---~---
      #--- Standardise day of year
      doy_jan        = ValueINT[doy_likely] %wr% c(  1, 31)
      doy_feb        = ValueINT[doy_likely] %wr% c( 32, 59)
      doy_mar        = ValueINT[doy_likely] %wr% c( 60, 90)
      doy_apr        = ValueINT[doy_likely] %wr% c( 91,120)
      doy_may        = ValueINT[doy_likely] %wr% c(121,151)
      doy_jun        = ValueINT[doy_likely] %wr% c(152,181)
      doy_jul        = ValueINT[doy_likely] %wr% c(182,212)
      doy_aug        = ValueINT[doy_likely] %wr% c(213,243)
      doy_sep        = ValueINT[doy_likely] %wr% c(244,273)
      doy_oct        = ValueINT[doy_likely] %wr% c(274,304)
      doy_nov        = ValueINT[doy_likely] %wr% c(305,334)
      doy_dec        = ValueINT[doy_likely] %wr% c(335,366)
      Value[doy_jan] = "01 Jan"
      Value[doy_feb] = "02 Feb"
      Value[doy_mar] = "03 Mar"
      Value[doy_apr] = "04 Apr"
      Value[doy_may] = "05 May"
      Value[doy_jun] = "06 Jun"
      Value[doy_jul] = "07 Jul"
      Value[doy_aug] = "08 Aug"
      Value[doy_sep] = "09 Sep"
      Value[doy_oct] = "10 Oct"
      Value[doy_nov] = "11 Nov"
      Value[doy_dec] = "12 Dec"
      #--- Standardise month
      Value[mon_likely] = sprintf( "%2.2i_%s"
                                 , ValueINT[mon_likely]
                                 , month.abb[ValueINT[mon_likely]]
                                 )#end sprintf
      #---~---
      #   Month label (Accounts for English, Portuguese, Spanish, French and Dutch).
      #---~---
      mon_jan        = tolower(substr(Value,1,3)) %in% c("jan","ene")
      mon_feb        = tolower(substr(Value,1,3)) %in% c("feb","fev")
      mon_mar        = tolower(substr(Value,1,3)) %in% c("mar","maa")
      mon_apr        = ( tolower(substr(Value,1,3)) %in% c("apr","abr","avr")
                       | tolower(Value) %in% "mid-april"                      )
      mon_may        = tolower(substr(Value,1,3)) %in% c("may","mai","mei")
      mon_jun        = ( tolower(substr(Value,1,3)) %in% c("jun")
                       | tolower(substr(Value,1,4)) %in% c("juin") )
      mon_jul        = ( tolower(substr(Value,1,3)) %in% c("jul")
                       | tolower(substr(Value,1,4)) %in% c("juil") )
      mon_aug        = tolower(substr(Value,1,3)) %in% c("aug","ago","aou")
      mon_sep        = tolower(substr(Value,1,3)) %in% c("sep","set")
      mon_oct        = tolower(substr(Value,1,3)) %in% c("oct","out","okt")
      mon_nov        = tolower(substr(Value,1,3)) %in% c("nov")
      mon_dec        = tolower(substr(Value,1,3)) %in% c("dec","dez","dic")
      Value[mon_jan] = "01 Jan"
      Value[mon_feb] = "02 Feb"
      Value[mon_mar] = "03 Mar"
      Value[mon_apr] = "04 Apr"
      Value[mon_may] = "05 May"
      Value[mon_jun] = "06 Jun"
      Value[mon_jul] = "07 Jul"
      Value[mon_aug] = "08 Aug"
      Value[mon_sep] = "09 Sep"
      Value[mon_oct] = "10 Oct"
      Value[mon_nov] = "11 Nov"
      Value[mon_dec] = "12 Dec"
      monlab_sel     = ( mon_jan | mon_feb | mon_mar | mon_apr
                       | mon_may | mon_jun | mon_jul | mon_aug
                       | mon_sep | mon_oct | mon_nov | mon_dec )
      #---~---


      #---~---
      #   Seed phenology may be provided in less accurate terms
      #---~---
      LCValue   = tolower(Value)
      spring    = LCValue %in% c("spring","early spring","mid spring","late spring"
                                ,"end of early spring","end of mid spring"
                                ,"start of early spring","start of mid spring"
                                )#end c
      sprsum    = LCValue %in% c("spring-summer","spring; summer")
      sprsumaut = LCValue %in% c("spring-autumn","spring-fall","spring; summer; autumn")
      spraut    = LCValue %in% c("spring; autumn")
      summer    = LCValue %in% c("summer","early summer","mid summer","late summer"
                                ,"midsummer","end of early summer"
                                ,"start of early summer")
      sumaut    = LCValue %in% c("summer-autumn","summer-fall","summer; autumn")
      sumautwin = LCValue %in% c("summer-winter","summer; autumn; winter")
      autumn    = LCValue %in% c("autumn","early autumn","mid autumn","late autumn"
                                ,"fall"  ,"early fall"  ,"mid fall"  ,"late fall"  )
      autwin    = LCValue %in% c("autumn-winter","fall-winter","autumn; winter")
      autwinspr = LCValue %in% c("autumn-spring","fall-spring","spring; autumn; winter")
      winter    = LCValue %in% c("winter","early winter","mid winter","late winter"
                                ,"pre-spring")
      winspr    = LCValue %in% c("winter-spring","spring; winter")
      winsprsum = LCValue %in% c("winter-summer","spring; summer; winter")
      allyear   = LCValue %in% c("all year","year round","spring; summer; autumn; winter")
      allrain   = LCValue %in% c("after rain","any time with rain")
      sprrain   = LCValue %in% c("any time with rain; spring")
      sasrain   = LCValue %in% c("any time with rain; spring; summer")
      sumrain   = LCValue %in% c("any time with rain; summer")
      wind      = LCValue %in% c("wind")
      misc      = LCValue %in% c("irregular","variable","no main period")
      Value[spring   ] = "20 Spring"
      Value[sprsum   ] = "21 Spring-Summer"
      Value[sprsumaut] = "22 Spring-Summer-Autumn"
      Value[spraut   ] = "23 Spring-Autumn"
      Value[summer   ] = "30 Summer"
      Value[sumaut   ] = "31 Summer-Autumn"
      Value[sumautwin] = "32 Summer-Autumn-Winter"
      Value[autumn   ] = "40 Autumn"
      Value[autwin   ] = "41 Autumn-Winter"
      Value[autwinspr] = "42 Autumn-Winter-Spring"
      Value[winter   ] = "50 Winter"
      Value[winspr   ] = "51 Winter-Spring"
      Value[winsprsum] = "52 Winter-Spring-Summer"
      Value[allyear  ] = "00 AllYear"
      Value[allrain  ] = "60 Rain-AllYear"
      Value[sprrain  ] = "61 Rain-Spring"
      Value[sasrain  ] = "62 Rain-Spring-Summer"
      Value[sumrain  ] = "63 Rain-Summer"
      Value[wind     ] = "61 WindDriven"
      Value[misc     ] = "99 Irregular"
      #--- Summary of cases with seasons
      season_sel = ( spring   | sprsum   | sprsumaut | spraut
                   | summer   | sumaut   | sumautwin
                   | autumn   | autwin   | autwinspr
                   | winter   | winspr   | winsprsum
                   | allyear  | allrain  | sprrain   | sasrain  |  sumrain
                   |  wind    | misc     )
      #---~---

      #---~---
      #   In case phenology was not observed, set it to NA and make results invalid
      #---~---
      bad_sel        = tolower(Value) %in% c("not observed","indeterminate","no","yes")
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #---~---
      #   To account for all cases, set value to NA if it doesn't fall into any of the 
      # categories. In this case we do not set Valid to FALSE because we want to identify
      # missing cases.
      #---~---
      acc_sel          = doy_likely | mon_likely | monlab_sel | season_sel
      Value[! acc_sel] = NA_character_
      #---~---


   }else if(TraitID %in% c(343L)){
      #---~---
      #   Plant life form (Raunkiaer life form)
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value = tolower(Value)
      #---~---




      #---~---
      #   Expand single-letter codes to reduce ambiguity.
      #---~---
      IsAuthor              = ( ( AuthorName %in% "Valerie Raevel" ) 
                              & ( NameOrig %in% "life form" ) )
      IsTherophyte          = IsAuthor & ( Value %in% c("t") )
      IsChamaephyte         = IsAuthor & ( Value %in% c("c") )
      IsGeophyte            = IsAuthor & ( Value %in% c("h") )
      IsPhanerophyte        = IsAuthor & ( Value %in% c("p") )
      Value[IsTherophyte  ] = "therophyte"
      Value[IsChamaephyte ] = "chamaephyte"
      Value[IsGeophyte    ] = "geophyte"
      Value[IsPhanerophyte] = "phanerophyte"
      #---~---


      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      Discard        = 
         ( ! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE)
         | ( Value %in% c( "always chamaephyte; always geophyte; always hemicryptophyte"
                         , "always chamaephyte; always hemicryptophyte"
                         , "always chamaephyte; always hemicryptophyte; always hemiphanerophyte"
                         , "always chamaephyte; always hemiphanerophyte"
                         , "always geophyte; always hemicryptophyte; always hydrophyte"
                         , "always geophyte; always hydrophyte"
                         , "always geophyte; always therophyte"
                         , "always hemicryptophyte; always hemiphanerophyte"
                         , "always hemiphanerophyte; always nanophanerophyte"
                         , "always macrophanerophyte; always nanophanerophyte"
                         , "always macrophanerophyte; sometimes nanophanerophyte"
                         , "always pseudophanerophyte", "ch-h", "chamaephyte/phanerophyte"
                         , "chamaephyte (nano-phanerophyte)", "chamaephyte (therophyte)"
                         , "chamephyte/hemicryptophyte", "cryptophyte", "false", "g (h)"
                         , "g (hh)", "g(hh)", "geophyte (hemicryptophyte)", "h", "h (ch)"
                         , "h (g)", "h (hh)", "h-ch", "h(ch)", "h(g)", "h(hh)"
                         , "hemicryptophyte/chamaephyte", "hemicryptophyte/phanerophyte"
                         , "hemicryptophyte (therophyte)", "hh", "ht", "ht-h", "ht-t"
                         , "hydrophyte/geophyte", "hydrophyte/geophyte/hemicryptophyte"
                         , "hz", "liana", "macro-phanerophyte (nano-phanerophyte)"
                         , "mega- meso- and micro- phanerophyte"
                         , "mega-; meso- and microphanerophyte"
                         , "multiple apical growth ponits", "n.a."
                         , "nano-phanerophyte (macro-phanerophyte)", "no", "np", "p", "ph"
                         , "phanerophyte", "phanerophyte (mega-;meso-; nano-)"
                         , "phanerophytes", "shrub", "single basal growth point"
                         , "sometimes chamaephyte; always hemicryptophyte"
                         , "species that represent single apical or multiple apical", "t"
                         , "t-ch", "t-h", "t-h(hh)", "t-ht", "t-th", "t; ht", "t; ht; h"
                         , "therophyte (chamaephyte)", "therophyte (hemicryptophyte)"
                         , "tree", "true", "vascular parasite", "vascular semi-parasite"
                         , "yes"
                         )#end c
           )#end Value
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---

      #---~---
      #   Reduce the number of classes. Keep the main classes for most types, but
      # retain size distinction of phanerotypes and the habit distinction for 
      # cryptophytes
      #---~---
      IsAerophyte         = Value %in% c( "aerophyte")
      IsChamaephyte       = Value %in% c( "always chamaephyte"
                                        , "always chamaephyte; always geophyte"
                                        , "always chamaephyte; sometimes therophyte"
                                        , "ch", "cha", "chamaephyte", "chamaephytes"
                                        , "chamephyte", "geophyte/chamaephyte"
                                        )#end c
      IsEpiphyte          = Value %in% c( "ep", "epiphyte")
      IsGeophyte          = Value %in% c( "always geophyte"
                                        , "always geophyte; sometimes therophyte"
                                        , "bulbous geophyte", "g", "geophyte", "geophytes"
                                        , "non-bulbous geophyte"
                                        , "non-bulbous geophyte (rhizome; corm or tuber)"
                                        )#end c
      IsHelophyte         = Value %in% c( "helophyte")
      IsHemicryptophyte   = Value %in% c( "always geophyte; always hemicryptophyte"
                                        , "always hemicryptophyte"
                                        , "always hemicryptophyte; sometimes hydrophyte"
                                        , "always hemicryptophyte; sometimes therophyte"
                                        , "geophyte/hemicryptophyte", "hemicryptophyte"
                                        , "hemicryptophytes"
                                        , "sometimes geophyte; always hemicryptophyte"
                                        )#end c
      IsHydrophyte        = Value %in% c( "always hemicryptophyte; always hydrophyte"
                                        , "always hydrophyte"
                                        , "always hydrophyte; always therophyte"
                                        , "annual hydrophyte (aquatic therophyte)"
                                        , "hydrophyte", "hydrophyte/chamaephyte"
                                        , "hydrophyte/hemicryptophyte"
                                        , "perennial hydrophyte"
                                        , "perennial hydrophyte (perennial water plant)"
                                        , "therophyte/hydrophyte"
                                        )#end c
      IsMegaphanerophyte  = Value %in% c( "always macrophanerophyte", "macro-phanerophyte"
                                        , "megaphanerophyte")
      IsMesophanerophyte  = Value %in% c( "always hemiphanerophyte"
                                        , "deciduous meso-phanerophytes"
                                        , "evergreen meso-phanerophytes"
                                        , "phanerophyte (meso-)"
                                        )#end c
      IsMicrophanerophyte = Value %in% c( "micro-phanerophytes")
      IsNanophanerophyte  = Value %in% c( "always nanophanerophyte", "nanophanerophyt"
                                        , "nanophanerophyte", "nanophanerophyte (shrub)"
                                        , "nano-phanerophyte"
                                        )#end c
      IsTherophyte        = Value %in% c( "always chamaephyte; always therophyte"
                                        , "always hemicryptophyte; always therophyte"
                                        , "always therophyte", "chamephyte/therophyte"
                                        , "questionable hemicryptophyte; always therophyte"
                                        , "sometimes hemicryptophyte; always therophyte"
                                        , "terophytes", "therophyte", "therophyte "
                                        , "therophyte (annual land plant)"
                                        , "therophyte/chamaephyte", "therophyte/geophyte"
                                        , "therophyte/geophyte/hemicryptophyte"
                                        , "therophyte/hemicryptophyte"
                                        , "therophyte/hemicryptophyte/chamaephyte"
                                        , "therophytes"
                                        )#end c
      #---~---


      #---~---
      #   Assign standardised classes.
      #---~---
      Value[IsAerophyte        ] = "Aerophyte"
      Value[IsChamaephyte      ] = "Chamaephyte"
      Value[IsEpiphyte         ] = "Epiphyte"
      Value[IsGeophyte         ] = "Geophyte"
      Value[IsHelophyte        ] = "Helophyte"
      Value[IsHemicryptophyte  ] = "Hemicryptophyte"
      Value[IsHydrophyte       ] = "Hydrophyte"
      Value[IsMegaphanerophyte ] = "Megaphanerophyte"
      Value[IsMesophanerophyte ] = "Mesophanerophyte"
      Value[IsMicrophanerophyte] = "Microphanerophyte"
      Value[IsNanophanerophyte ] = "Nanophanerophyte"
      Value[IsTherophyte       ] = "Therophyte"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( IsAerophyte         |IsChamaephyte        | IsEpiphyte
                           | IsGeophyte          | IsHelophyte         | IsHemicryptophyte
                           | IsHydrophyte        | IsMegaphanerophyte  | IsMesophanerophyte
                           | IsMicrophanerophyte | IsNanophanerophyte  | IsTherophyte
                           )#end Valid
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---


   }else if(TraitID %in% c(409L)){
      #---~---
      #   Shoot carbon/nitrogen (C/N) ratio
      #---~---

      #--- Turn qualitative values invalid.
      qual_sel        = tolower(Value) %in% c("low","medium","high")
      Value[qual_sel] = NA_character_
      Valid[qual_sel] = FALSE
      VName[qual_sel] = NA_character_
      #---~---

   }else if(TraitID %in% c(479L)){
      #---~---
      #   Leaf photosynthesis CO2 compensation point
      #---~---

      #--- Make sure all data have the same units (umol/mol).
      umolomol_sel  = UnitOrig %in% c("micromol mol-1","ppm","umol mol-1")
      pa_sel        = UnitOrig %in% c("Pa")
      Value[pa_sel] = as.character(1./prefsea*mol.2.umol * (as.numeric(Value[pa_sel])))
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( umolomol_sel | pa_sel )
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---


   }else if(TraitID %in% c(596L)){
      #---~---
      #   Seed germination requirement
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---



      #---~---
      #   Some authors used codes for different dormancy classes.
      #---~---
      #   F. Stuart III Chapin, 3 classes:
      #   0 - germinate immediately
      #   1 - weak dormancy (typically germinate the year after produced)
      #   2 - strong dormancy (typically enter the buried seed pool)
      #---~---
      IsAuthor             = 
         ( ( AuthorName %in% "F. Stuart III Chapin" )
         & ( NameOrig   %in% "seed_dor" )
         )#end IsAuthor
      IsImmediate           = IsAuthor & ( Value %in% c("0") )
      IsShortDormant        = IsAuthor & ( Value %in% c("1") )
      IsLongDormant         = IsAuthor & ( Value %in% c("2") )
      Value[IsImmediate   ] = "no dormancy"
      Value[IsShortDormant] = "short dormancy"
      Value[IsLongDormant ] = "long dormancy"
      #---~---
      #   Henry Ford uses requirement lists for germination.
      #---~---
      IsAuthor   = AuthorName %in% "Henry Ford"
      IsChilling = ( IsAuthor
                   & ( NameOrig %in% "germination requirements 1. chilling" )
                   & ( Value    %in% c( "absolute", "partial") )
                   )#end IsAuthor
      IsLight    = ( IsAuthor
                   & ( NameOrig %in% "germination requirements 2. light" )
                   & ( Value    %in% c( "absolute", "partial") )
                   )#end IsAuthor
      IsDtTemp   = ( IsAuthor
                   & ( NameOrig %in% "germination requirements 3. temperature fluctuation" )
                   & ( Value    %in% c( "absolute", "partial") )
                   )#end IsAuthor
      Value[IsChilling] = "chilling"
      Value[IsLight   ] = "light"
      Value[IsDtTemp  ] = "temperature fluctuation"
      #---~---
      #   Walton Green uses yes/no system for chilling
      #---~---
      IsAuthor   = AuthorName %in% "Walton Green"
      IsChilling = ( IsAuthor
                   & ( NameOrig %in% "cold stratification required" )
                   & ( Value    %in% "yes"                          )
                   )#end IsChilling
      Value[IsChilling] = "chilling"
      #---~---
      #   Dominik Thom uses yes/no system for serotiny, and number of days since last
      # time the plant experienced temperatures below 5 degC
      #---~---
      IsAuthor     = AuthorName %in% "Dominik Thom"
      IsSerotinous = ( IsAuthor
                     & ( NameOrig %in% "serotiny"        )
                     & ( Value    %in% c("yes","partly") )
                     )#end IsSerotinous
      IsChilling   = ( IsAuthor
                     & ( NameOrig          %in% "seed chill requirement" )
                     & ( as.numeric(Value) %ge% 30                       )
                     )#end IsChilling
      Discard      = ( IsAuthor
                     & ( NameOrig %in% "serotiny" )
                     & ( Value    %in% c("no")    )
                     )#end IsSerotinous
      Value[IsSerotinous] = "fire"
      Value[IsChilling  ] = "chilling"
      Value[Discard     ] = NA_character_
      Valid[Discard     ] = FALSE
      VName[Discard     ] = NA_character_
      #---~---


      #---~---
      #   Assign classes for those cases provided with yes/no type of entry.
      #---~---
      IsSerotinous   = 
         ( ( NameOrig %in% c( "serotinous:1; non-s: 0", "serotiny (canopy seed banck)"
                            , "serotiny (canopy seed bank)"
                            )#end c
           )#end NameOrig
         & ( Value    %in% c("serotinous","yes") )
         )#end IsPyriscence
      Value[IsSerotinous  ] = "fire"
      #---~---


      #---~---
      #   Some input variables are ancillary or refer to classification algorithms or
      # information that is too detailed to retain. We discard these values.
      #---~---
      IsTMI = 
         NameOrig %in% c( "maximum temperature at which 50% seeds germinate"
                        , "maximum temperature of germination"
                        , "minimum temperature at which 50% seeds germinate"
                        , "minimum temperature germination"
                        , "canopy_openness_max"
                        , "canopy_openness_min"
                        , "closed_canopy_germination"
                        , "gap_germination"
                        , "after-ripening requirement"
                        )#end c
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---




      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      Discard        = 
         ( ( ! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE) )
         | ( Value %in% c( "no", "non-serotinous", "partial" ) )
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Reduce the number of classes for simplicity.
      #   In the case of shade tolerance, the original trait is light requirement, we
      # assume full-light plants are shade intolerant.
      #---~---
      IsImmediate    = Value %in% c( "all seed immediately viable", "no dormancy", "none"
                                   )#end c
      IsLongDormant  = Value %in% c("long dormancy")
      IsShortDormant = Value %in% c( "short dormancy"
                                   , "short dormancy only (i.e. between seasons or up to 5 years in soil bank)"
                                   )#end c
      IsSerotinous   = Value %in% c( "serotinous", "fire")
      IsChilling     = Value %in% c("chilling")
      IsLight        = Value %in% c("light")
      IsDtTemp       = Value %in% c("temperature fluctuation")
      #---~---




      #---~---
      #   Reduce the number of classes for simplicity. Add numbers so they are sorted.
      #---~---
      Value[IsImmediate   ] = "Immediate"
      Value[IsShortDormant] = "Short Dormancy"
      Value[IsLongDormant ] = "Long Dormancy"
      Value[IsSerotinous  ] = "Fire"
      Value[IsChilling    ] = "Chilling"
      Value[IsLight       ] = "Light"
      Value[IsDtTemp      ] = "Temperature Fluctuation"
      #---~---



      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( IsImmediate    | IsShortDormant | IsLongDormant
                                 | IsSerotinous   | IsChilling     | IsLight
                                 | IsDtTemp      
                                 )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---


   }else if(TraitID %in% c(617L,619L)){
      #---~---
      #   Bark carbon (C) content per bark dry mass
      #   Bark phosphorus (P) content per bark dry mass
      #---~---

      #--- Make sure all data have the same units (%).
      fr_sel        = UnitOrig %in% c("dimensionless","g/g")
      pc_sel        = UnitOrig %in% c("%","percent","g/100g")
      Value[fr_sel] = as.character(frac2pc * (as.numeric(Value[fr_sel])))
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( fr_sel | pc_sel )
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---


   }else if(TraitID %in% c(711L)){
      #---~---
      #   Leaf water capacitance
      #---~---


      #---~---
      #   Adam Roddy. They provided the data in multiple units, and also capacitance 
      # after turgor loss. Discard those.
      #---~---
      IsAuthor = 
         ( ( AuthorName %in% "Adam Roddy" )
         & ( NameOrig   %in% c( "leaf capacitance after turgor loss"
                              , "leaf capacitance after turgor loss per dry mass"
                              , "leaf capacitance before turgor loss per dry mass"
                              )#end c
           )#end NameOrig
         )#end IsAuthor
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---


      #---~---
      #   Lawren Sack. Keep only the capacitance at full turgor.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Lawren Sack" )
                 & ( NameOrig   %in% "leaf absolute capacitance per leaf area" )
                 )#end IsAuthor
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---

      #--- Make sure all data have the same units (MPa-1).
      oneompa_sel        = UnitOrig %in% c("1/MPa","MPa-1")
      #---~---


      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & oneompa_sel
      Value[! IsValid] = NA_character_
      #---~---

   }else if(TraitID %in% c(719L,3479L)){
      #---~---
      #   Xylem hydraulic vulnerability; xylem cavitation vulnerability; 
      # embolism vulnerability; (P20; P50; P80)
      #---~---


      #---~---
      #   First we ignore most ancillary data.
      #---~---
      IsTMI = NameOrig %in% c( "safety margin (mpa)"
                             , "dh mean hydraulic diameter (?m)"
                             , "embolism resistance"
                             , "p50 safety margin"
                             , "p88 safety margin"
                             , "pmin midday (mpa)"
                             , "pmin midday leaf"
                             , "pmin midday stem"
                             , "pmin predawn (mpa)"
                             , "slope"
                             , "vulnerability index sensu carlquist (vessel diamter / vessel frequency)"
                             , "vi vulnerabiltiy index"
                             , "vulnerability index (unitless): vi"
                             , "mean ?50 (f only) (mpa)"
                             , "shape parameter for the vulnerability curve; a"
                             , "reference as reported by choat et al. 2012 for their data"
                             , "references for global mean data"
                             )#end c
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---


      #---~---
      #   Ignore vulnerability curves (we may be able to recover them later).
      #---~---
      IsAuthor = AuthorName %in% "Stefano Manzoni"
      IsTMI    = 
         ( IsAuthor
         & ( NameOrig %in% c("fraction of remaining conductivity", "psi (-mpa)") )
         )#end IsTMI
      IsTMI = NameOrig %in% c( "safety margin (mpa)"
                             , "dh mean hydraulic diameter (?m)"
                             , "embolism resistance"
                             , "p50 safety margin"
                             , "p88 safety margin"
                             , "pmin midday (mpa)"
                             , "pmin midday leaf"
                             , "pmin midday stem"
                             , "pmin predawn (mpa)"
                             , "slope"
                             , "vulnerability index sensu carlquist (vessel diamter / vessel frequency)"
                             , "mean ?50 (f only) (mpa)"
                             , "shape parameter for the vulnerability curve; a"
                             , "reference as reported by choat et al. 2012 for their data"
                             , "references for global mean data"
                             )#end c
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---



      #---~---
      #   Some data sets put the reference instead of providing the values in the
      # trait. Ignore these entries.
      #---~---
      ref_sel        = ( grepl(pattern="etal\\."     ,x=Value)
                       | grepl(pattern="^Beikircher" ,x=Value)
                       | grepl(pattern="^Bond"       ,x=Value)
                       | grepl(pattern="^Burgess"    ,x=Value)
                       | grepl(pattern="^Cochard"    ,x=Value)
                       | grepl(pattern="^Cordero"    ,x=Value)
                       | grepl(pattern="^Drake"      ,x=Value)
                       | grepl(pattern="^Edwards"    ,x=Value)
                       | grepl(pattern="^Froend"     ,x=Value)
                       | grepl(pattern="^Hacke"      ,x=Value)
                       | grepl(pattern="^Herbette"   ,x=Value)
                       | grepl(pattern="^Jacobsen"   ,x=Value)
                       | grepl(pattern="^Kolb"       ,x=Value)
                       | grepl(pattern="^Lipp"       ,x=Value)
                       | grepl(pattern="^LoGullo"    ,x=Value)
                       | grepl(pattern="^LuGollo"    ,x=Value)
                       | grepl(pattern="^Machado"    ,x=Value)
                       | grepl(pattern="^Mencuccini" ,x=Value)
                       | grepl(pattern="^Pockman"    ,x=Value)
                       | grepl(pattern="^Pratt"      ,x=Value)
                       | grepl(pattern="^Prior"      ,x=Value)
                       | grepl(pattern="^Sobrado"    ,x=Value)
                       | grepl(pattern="^Sperry"     ,x=Value)
                       | grepl(pattern="^Stiller"    ,x=Value)
                       | grepl(pattern="^Tibbetts"   ,x=Value)
                       | grepl(pattern="^Tognetti"   ,x=Value)
                       | grepl(pattern="^Tyree"      ,x=Value)
                       | grepl(pattern="^Vogt"       ,x=Value)
                       | grepl(pattern="^Wikberg"    ,x=Value)
                       )#end ref_sel
      Value[ref_sel] = NA_character_
      Valid[ref_sel] = FALSE
      VName[ref_sel] = NA_character_
      #---~---

      #--- Make sure all data have the same units (MPa).
      MPa_sel          = UnitOrig %in% c("Mpa","MPa","-Mpa","-MPa")
      bad_sel          = UnitOrig %in% c("m","m/(1/m2)","micro m","%/Mpa","fraction")
      Value[MPa_sel]  = as.character(-1.            * abs(as.numeric(Value[MPa_sel])))
      Value[bad_sel]  = NA_character_
      Valid[bad_sel]  = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #---~---
      #   Set the vulnerability variables
      #---~---
      IsP12 = NameOrig %in% c( "p12 (mpa)")
      IsP20 = NameOrig %in% c( "stem p20" )
      IsP50 = NameOrig %in% c( "p50", "p50 (mpa)"
                             , "water potential at 50% loss of conductivity psi_50 (mpa)"
                             , "choat et al. 2012 reported ?50 (mpa)"
                             , "mean ?50 (with all) (mpa)"
                             , "stem p50"
                             )#end c
      IsP80 = NameOrig %in% c( "stem p80" )
      IsP88 = NameOrig %in% c( "p88", "p88 (mpa)")
      #---~---



      #---~---
      #   Assign output names.
      #---~---
      VName[IsP12] = OutputName["P12"]
      VName[IsP20] = OutputName["P20"]
      VName[IsP50] = OutputName["P50"]
      VName[IsP80] = OutputName["P80"]
      VName[IsP88] = OutputName["P88"]
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & MPa_sel
      Value[! IsValid] = NA_character_
      #---~---



   }else if(TraitID %in% c(748L)){
      #---~---
      #   Leaf hydraulic conductance
      #---~---


      #---~---
      #   First we ignore most ancillary data.
      #---~---
      IsTMI = ( ( AuthorName %in% "Lawren Sack" )
              & ( NameOrig %in% c( "leaf hydraulic conductance (bagged leaf method)"
                                 , "leaf hydraulic conductance (evaporative flux method)"
                                 , "leaf hydraulic conductance (high pressure flow meter)"
                                 , "petiole hydraulic conductance per leaf area"
                                 )#end c
                )#end NameOrig
              )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---

      #--- Make sure all data have the same units (mol s-1 MPa-1).
      mmolom2osompa_sel  = UnitOrig %in% c("mmol m-2 s-1 MPa-1")
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & mmolom2osompa_sel
      Value[! IsValid] = NA_character_
      #---~---

   }else if(TraitID %in% c(752L)){
      #---~---
      #   Plant hydraulic conductance
      #---~---



      #---~---
      #   Discard data that are not in absolute units.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Lawren Sack" )
                 & ( NameOrig   %in% "whole-plant leaf-specific hydraulic conductance" )
                 )#end IsAuthor
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---

      #--- Make sure all data have the same units (mol s-1 MPa-1).
      molosompa_sel      = UnitOrig %in% c("mol s-1 MPa-1")
      gosompa_sel        = UnitOrig %in% c("g Mpa s-1")
      Value[gosompa_sel] = as.character( mmh2oi * g2kg * as.numeric(Value[gosompa_sel]) )
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = 
         Valid & ( molosompa_sel | gosompa_sel )
      Value[! IsValid] = NA_character_
      #---~---

   }else if(TraitID %in% c(785L,809L,1181L)){
      #---~---
      #   Root dry mass per root fresh mass (root dry matter content; RDMC)
      #   Leaf carotenoid content per leaf dry mass
      #   Stem dry mass per stem fresh mass (stem dry matter content; StDMC)
      #---~---

      #--- Initialise molar mass vector for data sets that report ratio in mols.
      mmcomp = rep(NA_real_,times=length(Value))
      #---~---


      #---~---
      #   Rob Salguero-Gomez, convert carotenoid molar content to mass.
      #---~---
      IsAuthor         = ( ( TraitID    %in% 809L) 
                         & ( AuthorName %in% "Rob Salguero-Gomez")
                         & ( NameOrig   %in% "total carotenoids" )
                         )#end IsAuthor
      mmcomp[IsAuthor] = mmcarot
      #---~---



      #--- Make sure all data have the same units (mg/g).
      umolog_sel        = UnitOrig %in% c("umol/gDW")
      ugog_sel          = UnitOrig %in% c("micro g / g","micro g/g")
      mgog_sel          = UnitOrig %in% c("mg/g","mg g-1")
      gog_sel           = UnitOrig %in% c("g/g","g*g-1","mg/mg")
      pc_sel            = UnitOrig %in% c("Root DM/FM %")
      Value[umolog_sel] = as.character(mmcomp * mg2kg * mol.2.umol
                                                      * as.numeric(Value[umolog_sel]))
      Value[ugog_sel  ] = as.character(ug2mg          * as.numeric(Value[ugog_sel  ]))
      Value[gog_sel   ] = as.character(g2mg           * as.numeric(Value[gog_sel   ]))
      Value[pc_sel    ] = as.character(pc2frac * g2mg * as.numeric(Value[pc_sel    ]))
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = 
         Valid & ( umolog_sel | ugog_sel | mgog_sel | gog_sel | pc_sel )
      Value[! IsValid] = NA_character_
      #---~---



   }else if (TraitID %in% c(819L)){
      #---~---
      #   Plant resprouting capacity
      #---~---
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---



      #---~---
      #   Some authors used codes or yes/no flags for resprouting
      #---~---
      #---~---
      #   Johannes Cornelissen uses 0/1 flag (0 means resprouter!).
      #---~---
      IsAuthor             = 
         ( ( AuthorName %in% "Johannes Cornelissen" )
         & ( NameOrig   %in% "capacity to resprout after fire (0=yes)" )
         )#end IsAuthor
      IsNonResprouter        = IsAuthor & ( Value %in% c("1") )
      IsResprouter           = IsAuthor & ( Value %in% c("0") )
      Value[IsNonResprouter] = "non-resprouter"
      Value[IsResprouter   ] = "resprouter"
      #---~---
      #   Walton Green uses no/yes
      #---~---
      IsAuthor             = 
         ( AuthorName %in% "Walton Green" ) & ( NameOrig   %in% "resprout ability" )
      IsNonResprouter        = IsAuthor & ( Value %in% c("no" ) )
      IsResprouter           = IsAuthor & ( Value %in% c("yes") )
      Value[IsNonResprouter] = "non-resprouter"
      Value[IsResprouter   ] = "resprouter"
      #---~---
      #   Tianhua He uses yes/no system for resprouting
      #---~---
      IsAuthor             = 
         ( AuthorName %in% "Tianhua He" ) & ( NameOrig   %in% "respout capacity" )
      IsNonResprouter        = IsAuthor & ( Value %in% c("no" ) )
      IsResprouter           = IsAuthor & ( Value %in% c("yes") )
      Value[IsNonResprouter] = "non-resprouter"
      Value[IsResprouter   ] = "resprouter"
      #---~---
      #   Tianhua He also describes types, but "no" must be flagged as non-resprouter
      #---~---
      IsAuthor             =
         ( ( AuthorName %in% "Tianhua He" )
         & ( NameOrig   %in% "sprouting capacity after fire" )
         )#end IsAuthor
      IsNonResprouter        = IsAuthor & ( Value %in% c( "n", "no" ) )
      Value[IsNonResprouter] = "non-resprouter"
      #---~---
      #   Mario Liebergesell uses 0/1 flag (1 means resprouter).
      #---~---
      IsAuthor             = 
         ( AuthorName %in% "Mario Liebergesell" ) & ( NameOrig   %in% "resp.disturb" )
      IsNonResprouter        = IsAuthor & ( Value %in% c("0") )
      IsResprouter           = IsAuthor & ( Value %in% c("1") )
      Value[IsNonResprouter] = "non-resprouter"
      Value[IsResprouter   ] = "resprouter"
      #---~---
      #   Juli Pausas uses yes/no and high/low/no systems for resprouting
      #---~---
      IsAuthor             = 
         ( ( AuthorName %in% "Juli Pausas" )
         & ( NameOrig   %in% c( "postfire resprouting", "respclip", "respdist"
                              , "respfire"
                              )#end c
           )#end NameOrig
         )#end IsAuthor
      IsNonResprouter        = IsAuthor & ( Value %in% c("no") )
      IsLowResprouter        = IsAuthor & ( Value %in% c("low") )
      IsResprouter           = IsAuthor & ( Value %in% c("yes","high") )
      Value[IsNonResprouter] = "non-resprouter"
      Value[IsLowResprouter] = "limited resprouting"
      Value[IsResprouter   ] = "resprouter"
      #---~---
      #   Josep Penuelas uses yes/no system for resprouting
      #---~---
      IsAuthor             = 
         ( AuthorName %in% "Josep Penuelas" ) & ( NameOrig %in% c( "resprouting capacity") )
      IsNonResprouter        = IsAuthor & ( Value %in% c("no" ) )
      IsResprouter           = IsAuthor & ( Value %in% c("yes") )
      Value[IsNonResprouter] = "non-resprouter"
      Value[IsResprouter   ] = "resprouter"
      #---~---
      #   Vasco Silva uses yes/no system for resprouting
      #---~---
      IsAuthor             = 
         ( ( AuthorName %in% "Vasco Silva" )
         & ( NameOrig %in% c( "resprouting capacity after fire"
                            , "resprouting cpcity fter fire"
                            )#end c
           )#end NameOrig
         )#end IsAuthor
      IsNonResprouter        = IsAuthor & ( Value %in% c("no" ) )
      IsResprouter           = IsAuthor & ( Value %in% c("yes") )
      Value[IsNonResprouter] = "non-resprouter"
      Value[IsResprouter   ] = "resprouter"
      #---~---
      #   Dominik Thom uses high/moderate/low/no systems for resprouting
      #---~---
      IsAuthor             = 
         ( ( AuthorName %in% "Dominik Thom" )
         & ( NameOrig   %in% c("resprouting ability") )
         )#end IsAuthor
      IsNonResprouter        = IsAuthor & ( Value %in% c("none"           ) )
      IsLowResprouter        = IsAuthor & ( Value %in% c("low"            ) )
      IsResprouter           = IsAuthor & ( Value %in% c("moderate","high") )
      Value[IsNonResprouter] = "non-resprouter"
      Value[IsLowResprouter] = "limited resprouting"
      Value[IsResprouter   ] = "resprouter"
      #---~---


      #---~---
      #   Some input variables are ancillary or refer to classification algorithms or
      # information that is too detailed to retain. We discard these values.
      #---~---
      IsTMI = NameOrig %in% c( "reproduction")
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---


      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      Discard        = 
         ( ! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE)
         | ( Value %in% c( "fire-killed / resprouter (e;l)"
                         , "fire-killed / resprouter (l;r)", "no"
                         , "resprouter / fire-killed"
                         , "survives 100% scorch; resprouts from epicormic shoots & killed by 100% scorch; seed storage unknown"
                         , "survives by outgrowing fire regime as soon mature; saplings resprouting"
                         , "variable"
                         )#end c
           )#end Value
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Reduce the number of classes, using Clarke et al. (2013) main classes when 
      # possible. When species are identified as resprouters but the type is ambiguous
      # or unclear, assign to the resprouter category.
      # 
      #  Clarke, PJ, Lawes MJ, Midgley JJ et al., 2013: Resprouting as a key functional
      #     trait: how buds, protection and resources drive persistence after fire.
      #     New Phytol. 197, 19-35, doi:10.1111/nph.12001.
      #---~---
      IsNonResprouter = Value %in% c( "fire-killed"
                                    , "killed by 100% scorch; no seed storage in area"
                                    , "killed by fire", "nonresprouter", "non-resprouter"
                                    , "not resprouting", "not resprouting after defoliation"
                                    )#end c
      IsLowResprouter = Value %in% c( "limited resprouting")
      IsAerial        = Value %in% c( "epicormic sprouting", "epicormic", "resprouter (e)"
                                    , "survives 100% scorch; resprouts from epicormic shoots"
                                    , "survives 100% scorch; resprouts from epicormic shoots & survives 100% scorch; outgrowth of apic ..."
                                    , "survives 100% scorch; resprouts from epicormic shoots & survives 100% scorch; resprout location ..."
                                    , "survives 100% scorch; resprouts from epicormic shoots?"
                                 )#end c
      IsBasal         = Value %in% c( "lignotuberous sprouting", "resprouter (l)", "rh"
                                    , "survives 100% scorch; resprouts from basal stem buds (eg lignotuber)"
                                    , "survives 100% scorch; resprouts from basal stem buds (eg lignotuber) & survives 100% scorch; re ..."
                                    , "tiny lignotuber", "yes; from basal lignotuber"
                                    )#end c
      IsBelowGround   = Value %in% c( "large root", "resprouter (r)", "rootstock", "rt"
                                    , "survives 100% scorch; resprouts from root suckers or rhizomes"
                                    )#end c
      IsResprouter    = Value %in% c(  "epicormic and lignotuberous"
                                    , "lignotuberous; root", "resprout after fire"
                                    , "resprouter", "resprouter  (l; r)", "resprouter ("
                                    , "resprouter (e; l; r)", "resprouter (e; l)"
                                    , "resprouter (l; r)", "resprouting"
                                    , "resprouting after defoliation"
                                    )#end c
      #---~---


      #---~---
      #   Assign standardised classes.
      #---~---
      Value[IsNonResprouter    ] = "01 Non-resprouter"
      Value[IsLowResprouter    ] = "02 Limited Resprouting"
      Value[IsAerial           ] = "03 Resprouter: Aerial"
      Value[IsBasal            ] = "04 Resprouter: Basal"
      Value[IsBelowGround      ] = "05 Resprouter: Below-ground"
      Value[IsResprouter       ] = "06 Resprouter: Other"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( IsNonResprouter | IsLowResprouter | IsAerial
                           | IsBasal         | IsBelowGround   | IsResprouter
                           )#end Valid
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---


   }else if (TraitID %in% c(825L)){
      #---~---
      #   Species occurrence range: climate type.  Some authors used ambiguous terms or
      # very broad descriptions. I tried to make this as consistent as possible, based on
      # the site or coordinates associated with the climate term. Some seemingly inconsistent
      # assignments may be true inaccuracies from the original description, though some may
      # become inconsistent here because multiple authors used the same term. This may require
      # additional processing once all data have been loaded.
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value    = tolower(Value)
      #---~---

      #---~---
      #   Some authors used ambiguous language on climate (Colleen Iversen has the 
      # standardised climate in ancillary variable "Vegetation type / Biome", use that 
      # one instead).
      #---~---
      IsTMI = ( ( AuthorName %in% "Alessandro Bricca"      )
              | ( AuthorName %in% "Johannes Cornelissen"   )
              | ( AuthorName %in% "Will Cornwell"          )
              | ( AuthorName %in% "Joseph Craine"          )
              | ( AuthorName %in% "nicolas fanin"          )
              | ( AuthorName %in% "Gregoire Freschet"      )
              | ( AuthorName %in% "Colleen Iversen"        )
              | ( AuthorName %in% "Pengcheng He"           )
              | ( AuthorName %in% "Jens Kattge"            )
              | ( AuthorName %in% "Yuanzhi Li"             )
              | ( AuthorName %in% "Patrick Meir"           )
              | ( AuthorName %in% "Peter Reich"            )
              | ( AuthorName %in% "Nadejda Soudzilovskaia" )
              | ( AuthorName %in% "Ian Wright"             )
              )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---




      #---~---
      #   For simplicity, we use Koeppen climate classification to the extent possible
      #---~---
      IsAf  = Value %in% c( "af" )
      IsAm  = Value %in% c( "am" )
      IsAw  = Value %in% c( "aw" )
      IsBWh = Value %in% c( "bwh")
      IsBWk = Value %in% c( "bwk")
      IsBSh = Value %in% c( "bsh")
      IsBSk = Value %in% c( "bsk", "arid steppe cold")
      IsCsa = Value %in% c( "csa", "mediterranean: mild with dry; hot summer")
      IsCsb = Value %in% c( "csb")
      IsCsc = Value %in% c( "csc")
      IsCwa = Value %in% c( "cwa")
      IsCwb = Value %in% c( "cwb")
      IsCwc = Value %in% c( "cwc")
      IsCfa = Value %in% c( "cfa", "cfa - mesothermic"
                          , "cfa (mesothermic; subtropical humid)"
                          , "cfa climate type in koeppen's classification"
                          , "cfa's climate type in koeppen-greige's classification"
                          )#end c
      IsCfb = Value %in% c( "cfb", "cfb climate type in koeppen's classification")
      IsCfc = Value %in% c( "cfc")
      IsDsa = Value %in% c( "dsa")
      IsDsb = Value %in% c( "dsb")
      IsDsc = Value %in% c( "dsc")
      IsDsd = Value %in% c( "dsd")
      IsDwa = Value %in% c( "dwa")
      IsDwb = Value %in% c( "dwb")
      IsDwc = Value %in% c( "dwc")
      IsDwd = Value %in% c( "dwd")
      IsDfa = Value %in% c( "dfa")
      IsDfb = Value %in% c( "dfb")
      IsDfc = Value %in% c( "dfc")
      IsDfd = Value %in% c( "dfd")
      IsET  = Value %in% c( "et" , "tundra")
      IsEF  = Value %in% c( "ef" )
      #---~---

      #---~---
      #   Make data sets consistent.
      #---~---
      Value[IsAf ] = "Af"
      Value[IsAm ] = "Am"
      Value[IsAw ] = "Aw"
      Value[IsBWh] = "BWh"
      Value[IsBWk] = "BWk"
      Value[IsBSh] = "BSh"
      Value[IsBSk] = "BSk"
      Value[IsCsa] = "Csa"
      Value[IsCsb] = "Csb"
      Value[IsCsc] = "Csc"
      Value[IsCwa] = "Cwa"
      Value[IsCwb] = "Cwb"
      Value[IsCwc] = "Cwc"
      Value[IsCfa] = "Cfa"
      Value[IsCfb] = "Cfb"
      Value[IsCfc] = "Cfc"
      Value[IsDsa] = "Dsa"
      Value[IsDsb] = "Dsb"
      Value[IsDsc] = "Dsc"
      Value[IsDsd] = "Dsd"
      Value[IsDwa] = "Dwa"
      Value[IsDwb] = "Dwb"
      Value[IsDwc] = "Dwc"
      Value[IsDwd] = "Dwd"
      Value[IsDfa] = "Dfa"
      Value[IsDfb] = "Dfb"
      Value[IsDfc] = "Dfc"
      Value[IsDfd] = "Dfd"
      Value[IsET ] = "ET"
      Value[IsEF ] = "EF"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = ( Valid
                         & ( IsAf  | IsAm  | IsAw
                           | IsBWh | IsBWk | IsBSh | IsBSk
                           | IsCsa | IsCsb | IsCsc
                           | IsCwa | IsCwb | IsCwc
                           | IsCfa | IsCfb | IsCfc
                           | IsDsa | IsDsb | IsDsc | IsDsd
                           | IsDwa | IsDwb | IsDwc | IsDwd
                           | IsDfa | IsDfb | IsDfc | IsDfd
                           | IsET  | IsEF                  )
                         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---



   }else if (TraitID %in% c(833L)){
      #---~---
      #   Bark persistence (deciduous; persistent)
      #---~---

      #---~---
      #   Make input values lower case to make classification easier
      #---~---
      Value = tolower(Value)
      #---~---


      #---~---
      #   Discard non-informative classes and those without any text.
      #---~---
      Discard        = ( ! grepl(pattern="[a-z]",x=Value,ignore.case=TRUE) )
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Reduce the number of classes for simplicity
      #---~---
      IsPersistent   = Value %in% c("no")
      IsDeciduous    = Value %in% c("yes","chunk","fissured","strip")
      #---~---


      #---~---
      #   Apply classification. For now we lump together 
      # all the deciduous types into one category, because most entries do not distinguish
      # them. This can be revised in the future.
      #---~---
      Value[IsPersistent] = "Persistent"
      Value[IsDeciduous ] = "Deciduous"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( IsPersistent | IsDeciduous )
      Value[! IsValid] = NA_character_
      #---~---

   }else if (TraitID %in% c(1096L)){
      #---~---
      #   Wood (sapwood) specific conductivity (stem specific conductivity)
      #---~---


      #---~---
      #   Peter Hietz. They calculated the theoretical hydraulic conductivity based on the
      # vessel size using the Hagen-Poiseuille equation for laminar flow of fluids with
      # known viscosity. This approach yields substantially higher conductivity as it 
      # does not account for the impact of water flow through vessel walls in reducing the
      # conductivity.  We discard the data as they are not comparable to other measurements
      #---~---
      IsAuthor = ( ( AuthorName %in% "Peter Hietz" )
                 & ( NameOrig   %in% "hydraulic conductivity" )
                 )#end IsAuthor
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #---~---

      #--- Make sure all data have the same units (kg m-1 s-1 MPa-1).
      kgomosompa_sel        =  UnitOrig %in% c( "kg m-1 s-1 MPa-1", "kg m-1 s-1 MPa -1"
                                              , "kg m-1 MPa-1 s-1", "kg/m/s/Mpa"
                                              )#end c
      Value[kgomosompa_sel] = as.character( as.numeric(Value[kgomosompa_sel]) )
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( kgomosompa_sel)
      Value[! IsValid] = NA_character_
      #---~---

   }else if (TraitID %in% c(1098L)){
      #---~---
      #   Wood (sapwood) water storage capacity
      #---~---

      #--- Make sure all data have the same units (kg m-3 MPa-1).
      kgom3ompa_sel        =  UnitOrig %in% c( "kg m-3 MPa-1" )
      Value[kgom3ompa_sel] = as.character( as.numeric(Value[kgom3ompa_sel]) )
      #---~---



      #---~---
      #   In case some units have not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( kgom3ompa_sel)
      Value[! IsValid] = NA_character_
      #---~---

   }#end if (TraitID %in% c(6L,18L,21L,46L,773L,1777L,2545L,3106L,3107L))
   #---~---


   #--- Return the fixed strings.
   Answer = tidyr::tibble( Value = Value, Valid = Valid, VName = VName )
   return(Answer)
   #---~---
}#end function TRY_FixTrait_OrigValue_Str
#---~---



#---~---
#   This function fixes ancillary data when loading the original string value.  Four 
# inputs are needed.
#
# - DataID     -- Data ID given for this ancillary data set given  by the TRY data base.
#                 This must be a scalar.
# - Type       -- Variable type ("numeric","integer","character","logical"). This must
#                 be a scalar too.
# - AncilOrig  -- Original string values for this ancillary data. 
#                 Either a scalar or a vector
# - UnitOrig   -- Original string with units for this ancillary data set. Either a scalar
#                 or a vector, dimension must match AncilOrig.
# - NameOrig   -- Original name of trait. This is useful for separating multiple variables
#                 shared by the same ancillary variable group.
# - AuthorName -- Author name. This is useful for separating multiple variables shared by 
#                 the same TraitID.
# - UniqOutput -- Write unique values to this file. If NULL, data are not written.
# - OutputName -- Name to associate with output variable. Currently this is not used but
#                 it could be relevant if multiple quantities are linked to the same input
#                 ancillary name.
#---~---
TRY_FixAncil_OrigValue_Str <<- function(DataID,Type,AncilOrig,UnitOrig,NameOrig
                                       ,AuthorName,UniqOutput=NULL,OutputName=NULL){

   #---~---
   #   Make sure dimensions of input values make sense.
   #---~---
   CntDataID    = length(DataID   )
   CntType      = length(Type     )
   CntAncilOrig = length(AncilOrig)
   CntUnitOrig  = length(UnitOrig )
   if ( (CntDataID != 1L) || (CntType != 1L) || (CntAncilOrig != CntUnitOrig) ){
      cat0(" ")
      cat0("------------------")
      cat0("   FATAL ERROR!   ")
      cat0("------------------")
      cat0(" - Length of variable \"DataID\":     ",CntDataID   ,".")
      cat0(" - Length of variable \"Type\":       ",CntType     ,".")
      cat0(" - Length of variable \"AncilOrig\":  ",CntAncilOrig,".")
      cat0(" - Length of variable \"UnitOrig\":   ",CntUnitOrig ,".")
      cat0(" ")
      cat0(" Variables \"DataID\" and \"Type\" must be scalars.")
      cat0(" Dimensions of variables  \"AncilOrig\" and \"UnitOrig\" must match.")
      cat0("------------------")
      stop(" Inconsistent settings.")
   }#end if ( (CntDataID != 1L) || (CntType != 1L) || (CntAncilOrig != CntUnitOrig) )
   #---~---



   #---~---
   #   Output Name: by default this is based on the DataID.
   #---~---
   if (is.null(OutputName)) OutputName = sprintf("ancil_id_%4.4i",DataID)
   #---~---



   #---~---
   #   Attribute: values that can be listed for ancillary variable. Useful when variables
   # are categorical.
   #---~---
   OutAttribute = NULL
   #---~---



   #---~---
   #   Initialise vector with Values and Validity. We flag traits that should become
   # invalid due to known limitations in the input data. Some ancillary variables contain
   # information that should have been provided as traits. If this is the case, we update
   # the trait information.  Make sure that the trait is consistently defined between this
   # function and the function above.
   #---~---
   Value   = AncilOrig
   Valid   = ! is.na(Value)
   VName   = ifelse(test=Valid,yes=OutputName,no=NA_character_)
   TraitID = rep(NA_integer_  ,times=length(Value))
   Trait   = rep(NA_character_,times=length(Value))
   AncilID = rep(NA_integer_  ,times=length(Value))
   Ancil   = rep(NA_character_,times=length(Value))
   #---~---



   #---~---
   #   Flag for writing table of unique values.
   #---~---
   WriteUnique = ! is.null(UniqOutput)
   #---~---


   #---~---
   #   Some entries have odd characters.
   #---~---
   Value = gsub(pattern="\u0092",replacement="'",x=Value)
   Value = gsub(pattern="\u0096",replacement="-",x=Value)
   Value = gsub(pattern="\u00a0",replacement="" ,x=Value)
   Value = gsub(pattern="<92>"  ,replacement="'",x=Value)
   Value = gsub(pattern="<96>"  ,replacement="-",x=Value)
   Value = gsub(pattern="<a0>"  ,replacement="" ,x=Value)
   #---~---


   #---~---
   #   Make original name case insensitve
   #---~---
   NameOrig = tolower(NameOrig)
   #---~---



   #--- Initial changes to the trait, to remove spurious characters.
   if (Type %in% c("numeric","integer")){
      #---~---
      #   A basic requirement for numeric and integer traits is to have numbers...
      #---~---
      bad_sel        = ! grepl(pattern="[0-9]",x=Value)
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---



      #---~---
      #   Some data sets are too cryptic to be fixed. Set them to NA.
      #---~---
      bad_sel        = Value %in% c("254mm Jun-Aug")
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #---~---
      #   Some entries have lots of extra information.
      #---~---
      Value = gsub(pattern=" \\= light saturation$",replacement="",x=Value)
      Value = gsub(pattern="^juvenile;"            ,replacement="",x=Value)
      Value = gsub(pattern="mineral$"              ,replacement="",x=Value)
      Value = gsub(pattern="mineral\\ soils$"      ,replacement="",x=Value)
      Value = gsub(pattern=" across\\ sites$"      ,replacement="",x=Value)
      Value = gsub(pattern="\\ after\\ sowing(.*)" ,replacement="",x=Value)
      Value = gsub(pattern="^ambient; "            ,replacement="",x=Value)
      #---~---


      #---~---
      #   Some data sets use "minus" to represent negative numbers, replace them with "-"
      #---~---
      Value = gsub(pattern="^minus",replacement="-",x=Value)
      #---~---


      #---~---
      #   Some entries have lots of extra information.
      #---~---
      tmi_sel = Value %in% "seedlings; < 1/2 year"
      Value   [tmi_sel] = "<0.5"
      UnitOrig[tmi_sel] = "year"
      #---~---


      #---~---
      #   Some data sets use comma as separators, not decimals. We make sure these are removed
      # instead of replaced.
      #---~---
      IsAuthor       = ( AuthorName %in% "Tamir Klein" )
      IsVariable     = DataID %in% 92L
      del_sep        = ( IsAuthor
                       & IsVariable 
                       & grepl(pattern=";"  ,x=Value)
                       & grepl(pattern="\\~",x=Value)
                       )#end del_sep
      Value[del_sep] = gsub(pattern=";"  ,replacement="",x=Value[del_sep])
      Value[del_sep] = gsub(pattern="\\~",replacement="",x=Value[del_sep])
      #---~---


      #---~---
      #   Some authors used classes to define stand age.
      #---~---
      IsAuthor    = ( ( AuthorName %in% "Christian Wirth" )
                    & ( NameOrig   %in% "standageclass"   )
                    )#end IsAuthor
      IsOldGrowth = IsAuthor & ( tolower(Value) %in% c("overmature (>150 y)") )
      IsMature    = IsAuthor & ( tolower(Value) %in% c("mature (60-150 y)"  ) )
      IsPole      = IsAuthor & ( tolower(Value) %in% c("pole (10-60 y)"     ) )
      IsYoung     = IsAuthor & ( tolower(Value) %in% c("young (0-10 y)"     ) )
      IsUneven    = IsAuthor & ( tolower(Value) %in% "unevenaged"       )
      UnitOrig[IsAuthor] = "year"
      Value[IsOldGrowth] = ">150"
      Value[IsMature   ] = "60-150"
      Value[IsPole     ] = "10-60"
      Value[IsYoung    ] = "<10"
      Value[IsUneven   ] = NA_character_
      Valid[IsUneven   ] = FALSE
      VName[IsUneven   ] = NA_character_
      #---~---


      #---~---
      #   Number values. Make sure any comma or semi-colon becomes a dot
      # and remove spurious spaces, which could make otherwise valid values 
      # become NA.
      #---~---
      Value = gsub(pattern=",",replacement=".",x=Value)
      Value = gsub(pattern=";",replacement=".",x=Value)
      Value = gsub(pattern=" ",replacement="" ,x=Value)
      #---~---


      #---~---
      #   Remove spurious characters, including uncertainties
      #---~---
      Value = gsub(pattern=" "          ,replacement="" ,x=Value)
      Value = gsub(pattern=";"          ,replacement=".",x=Value)
      Value = gsub(pattern="\\(.*\\)"   ,replacement="" ,x=Value)
      Value = gsub(pattern="\\-\\(.*\\)",replacement="" ,x=Value)
      Value = gsub(pattern="\\+\\/\\-.*",replacement="" ,x=Value)
      Value = gsub(pattern="\\+\\-.*"   ,replacement="" ,x=Value)
      #---~---









      #---~---
      #   Inventory all types of data available from this data set
      #---~---
      #--- For simplicity, we only keep classes with letters.
      if (WriteUnique & any(Valid)){
         #--- Check whether to write a new file or continue previous.
         NewFile  = (! file.exists(UniqOutput) )
         #---~---



         #---~---
         #   Create a tibble with the data
         #---~---
         UniqName = tibble( AuthorName = AuthorName[Valid]
                          , NameOrig   = tolower(NameOrig[Valid])
                          , UnitOrig   = ifelse( test = is.na(UnitOrig[Valid])
                                               , yes  = "dimensionless"
                                               , no   = UnitOrig[Valid]
                                               )#end ifelse
                          )#end tibble
         #---~---

         #---~---
         #   Aggregate categorical data
         #---~---
         sort_uniq = function(x) sort(unique(x))
         nl_table  = function(x) tabulate(factor(x=x,levels=sort_uniq(x)))
         UniqName = 
            UniqName                                                 %>%
            group_by(NameOrig)                                       %>%
            reframe( AuthorName = commonest(AuthorName,na.rm=TRUE)
                   , CntUnit    = nl_table (UnitOrig)
                   , UnitOrig   = sort_uniq(UnitOrig)              ) %>%
            select(c("AuthorName","NameOrig","UnitOrig","CntUnit"))
         #---~---



         #---~---
         #   Write file
         #---~---
         dummy = write_delim( x         = UniqName
                            , file      = UniqOutput
                            , append    = ! NewFile
                            , col_names = NewFile
                            , quote     = "needed"
                            , delim     = ","
                            )#end write_delim
         #---~---
      }#end if (WriteUnique)
      #---~---


   }else if (Type %in% c("character")){
      #---~---
      # Character values. There shouldn't be any commas, but just in case, we
      # update them. Also, if there is any empty string, with replace them 
      # with NA_character_
      #---~---
      Value          = gsub(pattern=",",replacement=";",x=Value)
      bad_sel        = Value %in% c("","na","not available")
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---







      #---~---
      #   Inventory all types of data available from this data set
      #---~---
      #--- For simplicity, we only keep classes with letters.
      UseValue = Valid & grepl(pattern="[a-z]",x=tolower(Valid))
      if (WriteUnique & any(UseValue)){
         #--- Check whether to write a new file or continue previous.
         NewFile  = (! file.exists(UniqOutput) )
         #---~---



         #---~---
         #   Create a tibble with the data
         #---~---
         UniqName = tibble( AuthorName = AuthorName[UseValue]
                          , NameOrig   = tolower(NameOrig[UseValue])
                          , Value      = tolower(Value   [UseValue])
                          )#end tibble
         #---~---

         #---~---
         #   Aggregate categorical data
         #---~---
         sort_uniq = function(x) sort(unique(x))
         nl_table  = function(x) tabulate(factor(x=x,levels=sort_uniq(x)))
         UniqName  = 
            UniqName                                                   %>%
            group_by(NameOrig)                                         %>%
            reframe( AuthorName = commonest(AuthorName,na.rm=TRUE)
                   , CntValue   = nl_table (Value)
                   , Value      = sort_uniq(Value)                   ) %>%
            select(c("AuthorName","NameOrig","Value","CntValue"))
         #---~---



         #---~---
         #   Write file
         #---~---
         dummy = write_delim( x         = UniqName
                            , file      = UniqOutput
                            , append    = ! NewFile
                            , col_names = NewFile
                            , quote     = "needed"
                            , delim     = ","
                            )#end write_delim
         #---~---
      }#end if (WriteUnique)
      #---~---


   }else if (Type %in% c("date")){
      #---~---
      # Date values. We clear some values that do not make sense and replace them 
      # with NA_character_
      #---~---
      Value          = gsub(pattern=",",replacement=";",x=Value)
      bad_sel        = Value %in% c("","na","none","not available","not reported"
                                   ,"to","- to -","NDR")
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #---~---
      #   Many authors provide non-standard dates, date range, or season information without
      # providing dates.  We seek to standardise the dates to the extent possible (assuming that
      # time of year is more critical than the exact date or exact year). But these cases, we 
      # assign a specific time to keep the most critical information. In some cases, the 
      # information is too generic and we really cannot use it, so we replace the dates with NA.
      #---~---
      tmi_info = tidyr::tribble( ~replace     , ~pattern
                               , NA_character_, "12/02/XX"
                               , NA_character_, "1966/67"
                               , NA_character_, "1980/81"
                               , NA_character_, "spet. 1981/April 1982"
                               , NA_character_, "summer 1985/spring 1996"
                               , NA_character_, "monthly measurements from Spt. 89 to January 1991"
                               , NA_character_, "2005; pre-monsoon"
                               , NA_character_, "Jan2005 to Aug2006"
                               , NA_character_, "2006; pre-monsoon"
                               , NA_character_, "nov-dec 2007 and 2009; april-may 2009"
                               , "1901-05-26" , "510"
                               , "1904-01-19" , "1478"
                               , "1907-01-11" , "2566"
                               , "1907-01-12" , "2567"
                               , "1913-09-10" , "5000"
                               , "1915-01-02" , "5479"
                               , "1918-12-06" , "6913"
                               , "1974-05-31" , "May-June 1974"
                               , "1974-08-15" , "May-Nov. 1974"
                               , "1983-07-01" , "juni-juli 1983"
                               , "1986-08-06" , "Summer 1986"
                               , "1988-01-27" , "25-29.1.88"
                               , "1989-11-05" , "Fall 1989"
                               , "1992-03-31" , "March & April 1992"
                               , "1992-05-28" , "28.May 1992"
                               , "1992-07-28" , "28.July 1992"
                               , "1993-07-31" , "July/Aug. 1992/Aug. 1993"
                               , "1994-01-14" , "measurements taken on Nov 2 and 16 1993; Jan 26; Feb 2; 8; 15 and 27 1994; beginning 0700h and  ..."
                               , "1994-05-01" , "April & May 1994"
                               , "1996-06-21" , "21.6.11996"
                               , "1997-01-30" , "January-February 1997"
                               , "1997-08-06" , "summer 1997"
                               , "1998-08-06" , "mostly summer 1998; additional data from summer 1994 and 1999 where indicated"
                               , "1998-08-06" , "mostly summer 1998; additional data from summer 1997 where indicated"
                               , "1999-08-06" , "summer 1999"
                               , "1999-07-31" , "1999-(July/Aug.)"
                               , "2000-08-06" , "summer 1997 and 2000"
                               , "2001-08-06" , "summer 2001"
                               , "2001-08-06" , "summer 1997; 2000 and 2001"
                               , "2003-08-15" , "july; aug. 2002; july; aug.; sept. 2003"
                               , "2004-05-01" , "April may 2004"
                               , "2005-06-21" , "spring/summer 2005"
                               , "2005-08-06" , "Summer 2015"
                               , "2005-08-06" , "summer 2015"
                               , "2006-07-16" , "june; july august  2005 and 2006"
                               , "2006-07-31" , "July-August; 2005-2007"
                               , "2006-08-06" , "summer 2006-2007"
                               , "2007-08-06" , "summer 2007"
                               , "2007-10-16" , "August - December 2007; end of rain season"
                               , "2007-10-31" , "August 2007 - January 2008"
                               , "2008-02-06" , "first week of February; 2008"
                               , "2008-05-29" , "spring/early summer 2008"
                               , "2008-07-16" , "07/2007 and 07/2008"
                               , "2009-01-28" , "last week of January; 2009"
                               , "2009-02-27" , "1.12.2008-27.5.2009"
                               , "2009-03-31" , "March/April  2008/2009"
                               , "2009-07-01" , "june-July 2009"
                               , "2009-08-06" , "Summer 2009"
                               , "2010-07-16" , "July_2010"
                               , "2010-07-31" , "July 12 - August 19; 2010"
                               , "2011-06-15" , "201106"
                               , "2011-09-14" , "Spring 2011 (late Aug - early Oct)"
                               , "2011-10-13" , "10/11-14/11"
                               , "2012-05-01" , "April-May 2012"
                               , "2012-08-16" , "agus-12"
                               , "2012-09-30" , "second semester 2012"
                               , "2013-07-01" , "June-July 2013"
                               , "2013-07-15" , "09/07/2013-21/07/2013"
                               , "2013-07-16" , "May to August of 2012; May to September of 2013"
                               , "2013-06-10" , "2/06/2013-18/06/2013"
                               , "2014-11-02" , "02/112014"
                               , "2015-07-01" , "May-August 2014/2015"
                               , "2015-07-01" , "2014-2015 growing seasons"
                               , "2015-12-16" , "November 2015- January 2016"
                               , "2015-12-18" , "2015-12-17/2015-12-20"
                               , "2016-06-08" , "26/05/2016 - 21/06/2016"
                               , "2016-06-22" , "08/06/2016- 05/07/2016"
                               , "2016-07-21" , "June-July 2016"
                               , "2016-07-22" , "11/07/2016 - 03/08/2016"
                               , "2016-08-08" , "21/07/2016 - 26/08/2016"
                               , "2017-08-06" , "summer 2017"
                               , "2017-07-16" , "2016/17"
                               , "2018-08-06" , "summer 2018"
                               , "2019-04-17" , "13-21.04.2019"
                               , "2099-07-22" , "19-25 July"
                               , "2099-09-28" , "Sept - Oct."
                               )#end tidyr::tribble
      for (i in sequence(nrow(tmi_info))){
         i_pattern         = tmi_info$pattern[i]
         i_replace         = tmi_info$replace[i]
         tmi_sel           = Value %in% i_pattern
         Value   [tmi_sel] = i_replace
         Valid   [tmi_sel] = ! is.na(i_replace)
         if (is.na(i_replace)) VName   [tmi_sel] = NA_character_
      }#end for (i in sequence(nrow(tmi_info)))
      #---~---


      #---~---
      #   Remove some bogus characters, and remove extra details.
      #---~---
      Value = gsub(pattern="^\\/",replacement="",x=Value)
      Value = gsub(pattern="\\/$",replacement="",x=Value)
      Value = gsub(pattern="\\?" ,replacement="",x=Value)
      Value = gsub(pattern=";$"  ,replacement="",x=Value)
      Value = gsub(pattern="^\\-",replacement="",x=Value)
      Value = gsub(pattern="\\-$",replacement="",x=Value)
      Value = gsub( pattern     = "; during mid morning hours \\(9-11h\\. Solar time\\)"
                  , replacement = ""
                  , x           = Value
                  )#end gsub
      #---~---


      #---~---
      #   Some data sets provide year range.  For now we set them to NA, because 
      # the priority at this point is to get an idea of season, but this may change
      # in the future.
      #---~---
      suppressWarnings({
         # Most cases have a simple separator, but a few use multi-character keys.
         HasDash   = grepl(pattern="\\ \\-\\ ",x=Value)
         HasTo     = grepl(pattern="\\ to\\ " ,x=Value)
         YearLen   = ifelse( test = HasDash
                           , yes  = 11L
                           , no   = ifelse( test = HasTo, yes = 12L,no = 9L)
                           )#end ifelse
         YearZAPos = ifelse( test = HasDash
                           , yes  = 8L
                           , no   = ifelse( test = HasTo, yes = 9L,no = 6L)
                           )#end ifelse
         # Check if these are likely year ranges
         YearRange = 
            suppressWarnings( ( nchar(Value) %in% YearLen )
                            & ( is.finite(as.numeric(substring(Value,        1,      4))) )
                            & ( is.finite(as.numeric(substring(Value,YearZAPos,YearLen))) )
                            )#end suppressWarnings
      })#end suppressWarnings
      Value[YearRange] = NA_character_
      Valid[YearRange] = FALSE
      VName[YearRange] = NA_character_
      #---~---


      #---~---
      #   Some data sets used semi-colons, colons, or dots to separate year, month, and date
      #---~---
      IsAuthor        = AuthorName %in% c( "Daniel Hornstein"
                                         , "Jeffrey Chambers"
                                         , "Maowei Liang"
                                         , "Yan-Shih Lin"
                                         , "Emily Rollinson"
                                         , "Christian Rossi"
                                         , "Rob Salguero-Gomez"
                                         , "Michael Scherer-Lorenzen"
                                         , "Frank Schurr"
                                         , "Fritz Schweingruber"
                                         , "Ruben Tarifa"
                                         , "Fons van der Plas"
                                         )#end c
      Value[IsAuthor] = gsub(pattern=";"     ,replacement="-",x=Value[IsAuthor])
      Value[IsAuthor] = gsub(pattern="\\."   ,replacement="-",x=Value[IsAuthor])
      Value[IsAuthor] = gsub(pattern="_"     ,replacement="-",x=Value[IsAuthor])
      #---~---


      #---~---
      #   Some authors have unique values that are ambiguous, but from other data
      # sets it is possible to infer the date.
      #---~---
      IsAuthor       = AuthorName %in% c( "Quentin Read")
      Except         = IsAuthor & ( Value %in% "12.07.12" )
      Value[Except ] = "2012-07-12"
      #---~---


      #---~---
      #   Data sets that have two dates; remove the first date as the script cannot handle two dates 
      # at this point.
      #---~---
      #--- " to " is used for separating dates
      IsAuthor      = AuthorName %in% c("Giandiego Campetella","Colleen Iversen")
      to_sel        = grepl(pattern="to",x=Value) & IsAuthor
      Value[to_sel] = gsub(pattern="(.*)\\ to\\ ",replacement="",x=Value[to_sel])
      #--- ";" is used for separating dates
      IsAuthor      = AuthorName %in% c("Belinda Medlyn")
      to_sel        = grepl(pattern="(.*)[0-9];[0-9](.*)",x=Value) & IsAuthor
      Value[to_sel] = gsub(pattern="(.*)[0-9];",replacement="",x=Value[to_sel])
      #--- "/" is used for separating dates
      IsAuthor      = AuthorName %in% c("Frank Schurr")
      to_sel        = grepl(pattern="(.*)[0-9]\\/[0-9](.*)",x=Value) & IsAuthor
      Value[to_sel] = gsub(pattern="(.*)[0-9]\\/",replacement="",x=Value[to_sel])
      #---~---


      #--- Some have too much information on time. Keep only the date.
      tmi_sel        = grepl(pattern=":",x=Value)
      Value[tmi_sel] = gsub(pattern="\\ (.*)$",replacement="",x=Value[tmi_sel])
      #---~---


      #---~---
      #   Standardise separators to be dashes instead of slashes.
      #---~---
      Value = gsub(pattern="\\/\\/",replacement="-",x=Value)
      Value = gsub(pattern="\\/"   ,replacement="-",x=Value)
      #---~---


      #---~---
      #   Remove trailing separators.
      #---~---
      Value  = gsub(pattern="\\-\\-$",replacement="",x=Value)
      Value  = gsub(pattern="\\-$"   ,replacement="",x=Value)
      #---~---



      #---~---
      #   Some data sets use Roman numbers for months. 
      #   The order below avoids replacing I, V, X that are part of compound numbers.
      #---~---
      IsAuthor         = AuthorName %in% c( "Jitka Klimesova" )
      IsRoman          = ( IsAuthor 
                         & ( grepl(pattern="X\\ ",x=Value) | grepl(pattern="V\\ ",x=Value)
                           | grepl(pattern="I\\ ",x=Value) )
                         )#end IsRoman
      Value[IsRoman ] = gsub(pattern="XII"   ,replacement="12",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="XI"    ,replacement="11",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="IX"    ,replacement="09",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="VIII"  ,replacement="08",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="VII"   ,replacement="07",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="VII"   ,replacement="06",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="IV"    ,replacement="04",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="III"   ,replacement="03",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="II"    ,replacement="02",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="X"     ,replacement="10",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="V"     ,replacement="05",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="I"     ,replacement="01",x=Value[IsRoman ])
      Value[IsRoman ] = gsub(pattern="\\ "   ,replacement="-" ,x=Value[IsRoman ])
      #--- Standardise the separators.
      Value[IsAuthor] = gsub(pattern=" \\.\\ ",replacement="-" ,x=Value[IsAuthor])
      Value[IsAuthor] = gsub(pattern="\\.\\ " ,replacement="-" ,x=Value[IsAuthor])
      Value[IsAuthor] = gsub(pattern="\\."    ,replacement="-" ,x=Value[IsAuthor])
      Value[IsAuthor] = gsub(pattern="\\ "    ,replacement="-" ,x=Value[IsAuthor])
      #---~---


      #---~---
      #   Remove ordinal references.
      #---~---
      IsOrd        = ( grepl( pattern="1st\\ "    ,x=Value,ignore.case=TRUE)
                     | grepl( pattern="2nd\\ "    ,x=Value,ignore.case=TRUE)
                     | grepl( pattern="3rd\\ "    ,x=Value,ignore.case=TRUE)
                     | grepl( pattern="[0-9]th\\ ",x=Value,ignore.case=TRUE)
                     )#end IsOrd
      Value[IsOrd] = gsub( pattern     = "1st\\ "  
                         , replacement = "1 "
                         , x           = Value[IsOrd]
                         , ignore.case = TRUE
                         )#end gsub
      Value[IsOrd] = gsub( pattern     = "2nd\\ "
                         , replacement = "2 "
                         , x           = Value[IsOrd]
                         , ignore.case = TRUE
                         )#end gsub
      Value[IsOrd] = gsub( pattern     = "3rd\\ "
                         , replacement = "3 "
                         , x           = Value[IsOrd]
                         , ignore.case = TRUE
                         )#end gsub
      Value[IsOrd] = gsub( pattern     = "th\\ "
                         , replacement = " "
                         , x           = Value[IsOrd]
                         , ignore.case = TRUE
                         )#end gsub
      #---~---


      #---~---
      #   Remove remarks of uncertainty.
      #---~---
      IsAuthor        = AuthorName %in% c( "David Tng")
      Value[IsAuthor] = gsub( pattern     = "^about\\ "
                            , replacement = ""
                            , x           = Value[IsAuthor]
                            , ignore.case = TRUE
                            )#end gsub
      #---~---


      #---~---
      #   Remove semi-colons following numbers (e.g., June 15; 2023)
      #---~---
      Value = gsub(pattern="(^.*[0-9]);",replacement="\\1",x=Value)
      #---~---

      #---~---
      #   Remove month names and qualitative text.
      #---~---
      Value = gsub(pattern="^pr"            ,replacement="15" ,x=Value,ignore.case=TRUE)
      Value = gsub(pattern="^ult"           ,replacement="15" ,x=Value,ignore.case=TRUE)
      Value = gsub(pattern="^beginning\\ of",replacement="05" ,x=Value,ignore.case=TRUE)
      Value = gsub(pattern="^mid"           ,replacement="15" ,x=Value,ignore.case=TRUE)
      Value = gsub(pattern="^end\\ of"      ,replacement="25" ,x=Value,ignore.case=TRUE)
      Value = gsub(pattern="sept\\."        ,replacement="Sep",x=Value,ignore.case=TRUE)
      for (m in sequence(12L)){
          M_MonthName    = tolower(month.name[m])
          M_MonthAbb     = tolower(month.abb [m])
          M_MonthName_sc = paste0(M_MonthName,"\\;")
          M_MonthAbb_sc  = paste0(M_MonthAbb ,"\\;")
          M_MonthName_dt = paste0(M_MonthName,"\\.")
          M_MonthAbb_dt  = paste0(M_MonthAbb ,"\\.")
          M_Value        = sprintf("%2.2i",m)
          mon_sel        = ( grepl(pattern=M_MonthName_sc,x=Value,ignore.case=TRUE)
                           | grepl(pattern=M_MonthName_dt,x=Value,ignore.case=TRUE)
                           | grepl(pattern=M_MonthName   ,x=Value,ignore.case=TRUE)
                           | grepl(pattern=M_MonthAbb_sc ,x=Value,ignore.case=TRUE)
                           | grepl(pattern=M_MonthAbb_dt ,x=Value,ignore.case=TRUE)
                           | grepl(pattern=M_MonthAbb    ,x=Value,ignore.case=TRUE)
                           )#end mon_sel
          Value[mon_sel] = gsub( pattern     = "\\ \\ "
                               , replacement = "-"
                               , x           = Value[mon_sel]
                               )#end gsub
          Value[mon_sel] = gsub( pattern     = "\\ "
                               , replacement = "-"
                               , x           = Value[mon_sel]
                               )#end gsub
          Value[mon_sel] = gsub( pattern     = M_MonthName_sc
                               , replacement = M_Value
                               , x           = Value[mon_sel]
                               , ignore.case = TRUE
                               )#end gsub
          Value[mon_sel] = gsub( pattern     = M_MonthName_dt
                               , replacement = M_Value
                               , x           = Value[mon_sel]
                               , ignore.case = TRUE
                               )#end gsub
          Value[mon_sel] = gsub( pattern     = M_MonthName
                               , replacement = M_Value
                               , x           = Value[mon_sel]
                               , ignore.case = TRUE
                               )#end gsub
          Value[mon_sel] = gsub( pattern     = M_MonthAbb_sc
                               , replacement = M_Value
                               , x           = Value[mon_sel]
                               , ignore.case = TRUE
                               )#end gsub
          Value[mon_sel] = gsub( pattern     = M_MonthAbb_dt
                               , replacement = M_Value
                               , x           = Value[mon_sel]
                               , ignore.case = TRUE
                               )#end gsub
          Value[mon_sel] = gsub( pattern     = M_MonthAbb
                               , replacement = M_Value
                               , x           = Value[mon_sel]
                               , ignore.case = TRUE
                               )#end gsub
      }#end for (m in sequence(12L))
      #---~---
   }else{
      # Other types. Not really sure what to do other than replacing
      # commas with semi-colons.
      Value = gsub(pattern=",",replacement=";",x=Value)
   }#end if (Type %in% c("numeric","integer"))
   #---~---


   #---~---
   #   Group data sets based on their main units.
   #---~---
   #   Coordinates
   AncilCoord      = DataID %in% c(59L,60L,4704L,4705L,4706L,4707L)
   #   Temperature
   AncilTemp       = DataID %in% c(62L,1665L,1666L,6692L,6693L,6932L,6936L)
   #   Water flux (precipitation, PET, etc).
   AncilWater      = DataID %in% c(80L,92L)
   #   CO2 variables.
   AncilCO2        = DataID %in% c(323L)
   #   Pressure
   AncilPres       = DataID %in% c(3801L)
   #   Length
   AncilLength     = DataID %in% c(274L,7042L)
   #   Area indices
   AncilXAI        = DataID %in% c(201L)
   #   Date (calendar dates)
   AncilDate       = DataID %in% c(212L,241L,595L,696L,6601L)
   #   Time (elapsed time)
   AncilTime       = DataID %in% c(413L,976L,1414L,1832L,3031L,3885L,4696L)
   #   Radiation
   AncilRad        = DataID %in% c( 88L, 321L, 340L)
   #   Relative humidity
   AncilHumid      = DataID %in% c(326L)
   #   Country
   AncilCountry    = DataID %in% c(1412L)
   #   Continent
   AncilContinent  = DataID %in% c(1413L)
   #   Disturbance 
   AncilDisturb    = DataID %in% c(496L)
   #   Sun/shade flag
   AncilSunShade   = DataID %in% c(210L,443L,766L,2111L)
   #   Biome
   AncilBiome      = DataID %in% c(193L,202L)
   #   Attributed growth form
   AncilGrowth     = DataID %in% c(6551L)
   #   Experiment
   AncilExperiment = DataID %in% c(238L, 308L, 319L, 324L, 327L, 363L, 490L, 767L, 4052L, 4695L)
   #---~---



   #---~---
   #    The following block is a long list of cases specific for each ancillary data.
   # This block may need revision if more ancillary data are needed and as more data are
   # contributed to TRY data base.
   #---~---
   if (AncilCoord){
      #---~---
      #   Longitude (decimal degrees)
      #---~---

      #--- Make sure all data have the same units (degC).
      deg_sel        = UnitOrig %in% c("decimal_degrees")
      #---~---


      #--- Delete data without units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! (deg_sel)
      Value[unmiss_sel] = NA_character_
      #---~---

   }else if (AncilTemp){
      #---~---
      #   Air temperature during measurement (Tair)
      #   Leaf temperature at Asat measurement (saturating light and ambient  CO2)
      #   Leaf temperature at Amax measurement (saturating light and saturating CO2)
      #   Leaf temperature during measurement (Tleaf)
      #   Leaf temperature for photosynthetic measurements
      #   Leaf temperature for respiration measurements
      #   Mean annual temperature (MAT)
      #---~---

      #--- Identify variables for editing some entries.
      IsTAir  = DataID %in% 1665L
      IsTLeaf = DataID %in% c(1666L,6692L,6693L,6932L,6936L)
      IsMAT   = DataID %in% 62L
      #---~---

      #--- Remove spurious characters.
      #---~---



      #---~---
      #   Some authors did not provide units, but the values are unequivocally degC.
      #---~---
      IsAuthor             = 
         AuthorName %in% c("Courtney Campany","nicolas fanin","Pengcheng He", "Chris Lusk"
                          , "Adam Martin")
      AssumeDegC           = IsAuthor & IsMAT
      UnitOrig[AssumeDegC] = "degC"
      #---~---
      IsAuthor             = 
         AuthorName %in% c("Adam Martin")
      AssumeDegC           = IsAuthor & IsTLeaf
      UnitOrig[AssumeDegC] = "degC"
      #---~---
      IsAuthor             = 
         AuthorName %in% c("Mark van Kleunen")
      AssumeDegC           = IsAuthor & IsTAir
      UnitOrig[AssumeDegC] = "degC"
      #---~---



      #--- Remove spurious characters (but track those with units in the value).
      has_degC = ( grepl(pattern="oC$"     ,x=Value)
                 | grepl(pattern="degC$"   ,x=Value)
                 | grepl(pattern="[0-9]C$" ,x=Value) )
      Value  = gsub(pattern="oC$"     ,replacement="",x=Value)
      Value  = gsub(pattern="degC$"   ,replacement="",x=Value)
      Value  = gsub(pattern="C$"      ,replacement="",x=Value)
      #---~---


      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---


      #--- Flag missing units
      unmiss_sel = (! is.na(Value) ) & is.na(UnitOrig)
      #---~---


      #--- Make sure all data have the same units (degC).
      Author_degC       = c( "Anh Tuan Dang-Le"
                           , "Wenxuan Han"
                           , "Robert Jackson"
                           , "Steven Jansen"
                           , "Yusuke Onoda"
                           , "Michael Scherer-Lorenzen"
                           , "Fritz Schweingruber"
                           )#end c
      Author_degC10     = c( "Chris Baraloto"
                           )#end c
      degC_sel          = UnitOrig %in% c( "C", "deg" , "deg C", "degC", "degrees C"
                                         , "degrees celcius", "degrees_C", "oC", "(oC)"
                                         , "?C" )
      degC_sel          = ( has_degC
                          | degC_sel
                          | ( unmiss_sel & all(AuthorName[unmiss_sel] %in% Author_degC) )
                          )#end degC_sel
      degC10_sel        = ( unmiss_sel
                          & all(AuthorName[unmiss_sel] %in% Author_degC10)
                          & all(! grepl(pattern="\\.",x=Value[unmiss_sel]))
                          )#end degC10_sel
      degC10_sel        = degC10_sel | ( UnitOrig %in% "C*10" )
      Value[degC10_sel] = as.character( 0.1 * (as.numeric(Value[degC10_sel])) )
      #---~---


      #--- Delete data without units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! (degC_sel | degC10_sel)
      Value[unmiss_sel] = NA_character_
      #---~---
   }else if (AncilWater){
      #---~---
      #   Mean annual sum of potential evapotranspiration (PET)
      #   Mean sum of annual precipitation (PPT / MAP / TAP)
      #---~---


      #---- Useful logical tests for variable specific actions.
      IsPET = DataID %in% 92L
      IsMAP = DataID %in% 80L
      #---~---


      #---~---
      #   Some data have too little information to unambiguously identify units.
      #---~---
      IsAuthor       = 
         AuthorName %in% c( "Michelle Leishman", "nicolas fanin", "Chris Lusk"
                          , "Adam Martin" )
      bad_sel        = IsAuthor & IsMAP
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #---~---
      #   Some authors did not provide units, but the values are unequivocally mm.
      #---~---
      IsAuthor           = AuthorName %in% "Courtney Campany"
      AssumeMM           = IsAuthor & IsMAP
      UnitOrig[AssumeMM] = "mm"
      #---~---



      #--- Remove spurious characters (but track those with units in the value).
      has_mm = ( grepl(pattern="mm$"     ,x=Value)
               | grepl(pattern="mm.$"    ,x=Value) )
      has_cm = ( grepl(pattern="cm$"     ,x=Value)
               | grepl(pattern="cm.$"    ,x=Value)
               | grepl(pattern="cn$"     ,x=Value)
               | grepl(pattern="cn.$"    ,x=Value)
               | grepl(pattern="c$"      ,x=Value)
               | grepl(pattern="vm$"     ,x=Value) )
      Value  = gsub(pattern="mm$"     ,replacement="",x=Value)
      Value  = gsub(pattern="mm.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="mm$"     ,replacement="",x=Value)
      Value  = gsub(pattern="mm.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="cn$"     ,replacement="",x=Value)
      Value  = gsub(pattern="cn.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="cm$"     ,replacement="",x=Value)
      Value  = gsub(pattern="cm.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="c$"      ,replacement="",x=Value)
      #---~---


      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---

      #--- Flag missing units
      unmiss_sel = (! is.na(Value) ) & is.na(UnitOrig)
      #---~---


      #--- Set units for data that are very likely in mm/yr.
      Author_mmoyr        = c( "Chris Baraloto"
                             , "Anh Tuan Dang-Le"
                             , "Wenxuan Han"
                             , "Thomas Hickler"
                             , "Robert Jackson"
                             , "Steven Jansen"
                             , "Yusuke Onoda"
                             , "Charles Price"
                             , "Peter Reich"
                             , "Michael Scherer-Lorenzen"
                             , "Fritz Schweingruber"
                             )#end c
      mmoyr_sel           = ( unmiss_sel
                             & all(AuthorName[unmiss_sel] %in% Author_mmoyr)
                             )#end mmoyr_sel
      UnitOrig[mmoyr_sel] = "mm yr-1"
      #---~---


      #--- Make sure all data have the same units (mm/yr). Discard percent data
      mmoyr_sel        = 
         has_mm | ( UnitOrig %in% c("(mm)","mm","mm ?","mm/y","mm/yr","mm/year"
                                   ,"mm / yr","mm yr-1","mm a-1")    )
      cmoyr_sel        = has_cm | ( UnitOrig %in% c("cm") )
      mmomo_sel        = UnitOrig %in% c("mm month-1")
      Value[cmoyr_sel] = as.character( cm.2.mm * (as.numeric(Value[cmoyr_sel])) )
      Value[mmomo_sel] = as.character( yr.mon  * (as.numeric(Value[mmomo_sel])) )
      #---~---


      #--- Delete data without units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! (mmoyr_sel | cmoyr_sel | mmomo_sel)
      Value[unmiss_sel] = NA_character_
      #---~---

   }else if (AncilCO2){
      #---~---
      #   Atmospheric CO2 concentration during measurement (Ca)
      #---~---



      #---~---
      #   Some authors did not provide units, but the values are unequivocally umol mol-1.
      #---~---
      IsAuthor                 = AuthorName %in% c("Adam Martin", "Mark van Kleunen")
      AssumeuMoloMol           = IsAuthor & (! is.na(Value) ) & is.na(UnitOrig)
      UnitOrig[AssumeuMoloMol] = "umol mol-1"
      #---~---



      #--- Remove spurious characters (but track those with units in the value).
      has_umolomol = grepl(pattern="umolmol-1$"               ,x=Value)
      Value        = gsub (pattern="umolmol-1$",replacement="",x=Value)
      #---~---

      #--- Remove spurious characters.
      Value = gsub(pattern="\\?",replacement="",x=Value)
      #---~---


      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---


      #--- Make sure all data have the same units (umol mol-1).
      umolomol_sel     =
         ( has_umolomol
         | ( UnitOrig %in% c( "?mol mol-1", "micromol CO2 m-1", "micromol CO2 mol-1"
                            , "micro mol mol-1", "micromol mol-1", "ppm", "umol mol-1"
                            , "umol mol-1 also the same as CO2 sample" )
           )#end UnitOrig
         )#end umolomol_sel
      #---~---


      #--- Delete data without known units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! (umolomol_sel)
      Value[unmiss_sel] = NA_character_
      #---~---

   }else if (AncilPres){
      #---~---
      #   Atmospheric pressure
      #---~---

      #--- Remove spurious characters.
      #---~---


      #--- Make sure all data have the same units (hPa). Discard percent data
      kPa_sel        = UnitOrig %in% c("kPa")
      Pa_sel         = UnitOrig %in% c("Pa")
      bad_sel        = UnitOrig %in% c("mV")
      Value[kPa_sel] = as.character( kPa.2.hPa * (as.numeric(Value[kPa_sel])) )
      Value[Pa_sel ] = as.character( Pa.2.hPa  * (as.numeric(Value[Pa_sel ])) )
      #---~---

      #--- Discard spurious data
      Value[bad_sel]  = NA_character_
      Valid[bad_sel]  = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #--- Delete data without known units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! (kPa_sel | Pa_sel | bad_sel)
      Value[unmiss_sel] = NA_character_
      #---~---

   }else if (AncilLength){
      #---~---
      #   Soil depth
      #   Ecosystem rooting depth
      #---~---


      #---~---
      #   Some data have values but no units. Assign units based on references whenever possible.
      #---~---
      IsUnitMiss         = is.na(UnitOrig)
      IsSoilDepth        = DataID %in% c(274L)
      IsRootDepth        = DataID %in% c(7042L)
      AssumeCm           = ( IsUnitMiss
                           & ( IsSoilDepth & (AuthorName %in% "Coline Boonman"  ) )
                           )#end UnitMiss
      UnitOrig[AssumeCm] = "cm"
      #---~---



      #--- Remove spurious characters (but track those with cm in the value).
      has_cm = ( grepl(pattern="cm$"     ,x=Value)
               | grepl(pattern="cm.$"    ,x=Value)
               | grepl(pattern="cn$"     ,x=Value)
               | grepl(pattern="cn.$"    ,x=Value)
               | grepl(pattern="c$"      ,x=Value)
               | grepl(pattern="vm$"     ,x=Value) )
      has_m  = ( grepl(pattern="[0-9]m$" ,x=Value) 
               | grepl(pattern="[0-9]n$" ,x=Value) 
               | grepl(pattern="[0-9]m.$",x=Value) )
      Value  = gsub(pattern="cn$"     ,replacement="",x=Value)
      Value  = gsub(pattern="cn.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="cm$"     ,replacement="",x=Value)
      Value  = gsub(pattern="cm.$"    ,replacement="",x=Value)
      Value  = gsub(pattern="c$"      ,replacement="",x=Value)
      Value  = gsub(pattern="m$"      ,replacement="",x=Value)
      Value  = gsub(pattern="n$"      ,replacement="",x=Value)
      Value  = gsub(pattern="m.$"     ,replacement="",x=Value)
      #---~---


      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---

      #--- Make sure all data have the same units (m).
      m_sel         = has_m  | ( UnitOrig %in% c("m" ) )
      cm_sel        = has_cm | ( UnitOrig %in% c("cm") )
      Value[cm_sel] = as.character( cm.2.m * (as.numeric(Value[cm_sel])) )
      #---~---


      #--- Delete data without units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! (m_sel | cm_sel)
      Value[unmiss_sel] = NA_character_
      #---~---

   }else if (AncilXAI){
      #---~---
      #   Leaf area index of the site (LAI)
      #---~---


      #--- Flag missing units
      unmiss_sel = (! is.na(Value) ) & is.na(UnitOrig)
      #---~---


      #--- Assume likely units based on value range.
      Author_m2om2        = c( "Kevin Karbstein"
                             , "Angela Moles"
                             , "Lawren Sack"
                             , "Christian Wirth"
                             )#end c
      m2om2_sel           = ( unmiss_sel
                            & all(AuthorName[unmiss_sel] %in% Author_m2om2)
                            )#end sel
      UnitOrig[m2om2_sel] = "m2 m-2"
      #---~---

      #--- Remove spurious characters.
      #---~---

      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---

      #--- Make sure all data have the same units (m2/m2).
      m2om2_sel        = UnitOrig %in% c("m2 m-2","m2/m2")
      #---~---


      #--- Delete data without units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! (m2om2_sel)
      Value[unmiss_sel] = NA_character_
      #---~---

   }else if (AncilDate){
      #---~---
      #   Measurement date / sampling date
      #   Measurement date: year
      #   Sampling or measurement date standardized
      #   Sampling date
      #   Sampling date: year
      #---~---


      #---~---
      #   Some data sets have date collection in the future or information that do not seem to be
      # dates. Ignore all information from these authors
      #---~---
      bad_sel        = ( ( AuthorName %in% c( "Pedro Higuchi" 
                                            , "Adam Martin" 
                                            , "Marina Scalon"  ) )
                       & ( DataID     %in% c(241L,6601L) )
                       )#end bad_sel
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #---~---
      #   Moises Mendez Toribio. The data have trait dates into the future, likely a result
      # of dragging Excel cells to fill in multiple rows. Considering the information 
      # available in the publication, we assume that the dates were supposed to be 
      # Sep-Nov 2012--2013
      #---~---
      IsAuthor       = AuthorName %in% "Moises Mendez Toribio"
      IsExcel        = IsAuthor & grepl(pattern="^sep-nov; 2012-",x=AncilOrig)
      Value[IsExcel] = "2013-10-16"
      #---~---


      #---~---
      #   Mark van Kleunen. The data have trait dates into the future, likely a result
      # of dragging Excel cells to fill in multiple rows. Considering the information 
      # available in the publication, we assume that the dates were supposed to be 
      # 2012-09-25 through 2012-10-01
      #---~---
      IsAuthor       = AuthorName %in% "Mark van Kleunen"
      IsExcel        = IsAuthor & grepl(pattern="^25\\/09\\/2012\\ -\\ 01\\/10",x=AncilOrig)
      Value[IsExcel] = "2012-09-28"
      #---~---


      #---~---
      #   Some dates have numbers that are too cryptic, but mixed with values that can be
      # used. Discard the cryptic data before proceeding.
      #---~---
      IsAuthor         = AuthorName %in% "Christian Wirth"
      IsCryptic        = IsAuthor & ( nchar(Value) %in% c(1L,2L) )
      Value[IsCryptic] = NA_character_
      Valid[IsCryptic] = FALSE
      VName[IsCryptic] = NA_character_
      #---~---


      #---~---
      #   Some authors provided dates mixed with facility names. Remove facility names.
      #---~---
      IsAuthor    = AuthorName %in% "Samantha Munroe"
      IsFacility  = IsAuthor & ( NameOrig %in% "facility-year" )
      ByePatterns = c( "CSIRO-", "University of Adelaide-", "University of Adelaide-"
                     , "Univeristy of Adelaide ", "University of Adeliade-")
      for (Pattern in ByePatterns){
         Value[IsFacility] = gsub(pattern=Pattern,replacement="",x=Value[IsFacility])
      }#end for (Pattern in ByePatterns)
      #---~---


      #--- Count number of dashes
      CntDash        = mapply( FUN      = function(x,...) length(gregexpr(text=x,...)[[1]])
                             , x        = Value
                             , MoreArgs = list(pattern="\\-")
                             )#end mapply
      names(CntDash) = NULL
      #---~---


      #---~---
      #   Some data sets have year and month, but not day. Append a mid-month date.
      #   Some entries only provide months, assign a dummy year and mid-month, which can still
      # be useful for seasonal analyses.
      #---~---
      #--- Data in year-month-day format
      IsAuthor          = AuthorName %in% c( "Gregoire Freschet"
                                           , "Greg Guerin"
                                           , "Mason Heberling"
                                           , "Colleen Iversen"
                                           , "Jens Kattge"
                                           , "Kim La Pierre"
                                           , "Maowei Liang"
                                           , "Juliana Medeiros"
                                           , "Patrick Meir"
                                           , "Bill Shipley"
                                           , "Christian Wirth"
                                           , "Kasia Ziemi&#324;ska"
                                           )#end c
      dmiss_sel         = ( (nchar(Value) %in% c(6L,7L))
                          & IsAuthor
                          & (substring(Value,5,5) %in% "-")
                          )#end dmiss_sel
      Value[dmiss_sel]  = paste0(Value[dmiss_sel],"-15")
      ymiss_sel         = ( (nchar(Value) %in% c(4L,5L))
                          & IsAuthor
                          & (substring(Value,3,3) %in% "-")
                          )#end ymiss_sel
      Value[ymiss_sel]  = paste0("2099-",Value[ymiss_sel])
      ydmiss_sel        = (nchar(Value) %in% c(2L)) & IsAuthor
      Value[ydmiss_sel] = paste0("2099-",Value[ydmiss_sel],"-15")
      #--- Data in yyyymmdd format
      IsAuthor          = AuthorName %in% c( "Joanne Sharpe"
                                           )#end c
      dmiss_sel         = ( (nchar(Value) %in% c(6L))
                          & IsAuthor
                          & (! grepl(pattern="\\-",x=Value) )
                          )#end dmiss_sel
      Value[dmiss_sel]  = paste0(Value[dmiss_sel],"15")
      #--- Data in day-month-year format (day is missing)
      IsAuthor          = AuthorName %in% c( "Carolyn Anderson"
                                           , "Courtney Campany"
                                           , "Emmanuele Farris"
                                           , "Rodolfo Gentili"
                                           , "Enqing Hou"
                                           , "Daniel Laughlin"
                                           , "Justin Luong"
                                           , "Jitka Klimesova"
                                           , "Michela Marignani"
                                           , "Ian McFadden"
                                           , "Akira Mori"
                                           , "Rachael Nolan"
                                           , "Valerie Raevel"
                                           , "Dina Ronzhina"
                                           , "Rob Salguero-Gomez"
                                           , "Ivan Simko"
                                           , "Linnea Smith"
                                           , "Alexandre Souza"
                                           , "Kris Verheyen"
                                           , "Han Wang"
                                           , "Nikolai Yankov"
                                           )#end c
      dmiss_sel         = ( IsAuthor
                          & ( CntDash %in% c(1L) )
                          & ( ( ( nchar(Value) %in% c(4L,6L) )
                              & ( substring(Value,2,2) %in% "-" )
                              )#end Value6L
                            | ( ( nchar(Value) %in% c(5L,7L) )
                              & ( substring(Value,3,3) %in% "-" )
                              )#end Value7L
                            )#end ncharValue
                          )#end IsAuthor
      Value[dmiss_sel]  = paste0("15-",Value[dmiss_sel])
      #--- Data in day-month-year format (year is missing)
      IsAuthor          = AuthorName %in% c( "Fons van der Plas"
                                           )#end c
      ymiss_sel         = ( IsAuthor
                          & ( CntDash %in% c(1L) )
                          & ( ( ( nchar(Value) %in% c(3L,4L) )
                              & ( substring(Value,2,2) %in% "-" )
                              )#end Value3-4L
                            | ( ( nchar(Value) %in% c(4L,5L) )
                              & ( substring(Value,3,3) %in% "-" )
                              )#end Value4-5L
                            )#end ncharValue
                          )#end IsAuthor
      Value[ymiss_sel]  = paste0(Value[ymiss_sel],"-2099")
      #---~---
      #--- Data in month-day-year format
      IsAuthor          = AuthorName %in% c( "Ulo Niinemets"
                                           , "Alexander Novakovskiy"
                                           , "Josep Penuelas"
                                           , "Victor Rolo Romero"
                                           , "Fritz Schweingruber"
                                           )#end c
      d4miss_sel        = ( IsAuthor
                          & ( CntDash %in% c(1L) )
                          & ( nchar(Value) %in% c(4L) )
                          & ( substring(Value,2,2) %in% "-" )
                          )#end IsAuthor
      Value[d4miss_sel] = paste0( substring(Value[d4miss_sel],1,1)
                                , "-15-"
                                , substring(Value[d4miss_sel],3,4)
                                )#end paste0
      d5miss_sel        = ( IsAuthor
                          & ( CntDash %in% c(1L) )
                          & ( nchar(Value) %in% c(5L) )
                          & ( substring(Value,3,3) %in% "-" )
                          )#end IsAuthor
      Value[d5miss_sel] = paste0( substring(Value[d5miss_sel],1,2)
                                , "-15-"
                                , substring(Value[d5miss_sel],4,5)
                                )#end paste0
      d6miss_sel        = ( IsAuthor
                          & ( CntDash %in% c(1L) )
                          & ( nchar(Value) %in% c(6L) )
                          & ( substring(Value,2,2) %in% "-" )
                          )#end IsAuthor
      Value[d6miss_sel] = paste0( substring(Value[d6miss_sel],1,1)
                                , "-15-"
                                , substring(Value[d6miss_sel],3,6)
                                )#end paste0
      d7miss_sel        = ( IsAuthor
                          & ( CntDash %in% c(1L) )
                          & ( nchar(Value) %in% c(7L) )
                          & ( substring(Value,3,3) %in% "-" )
                          )#end IsAuthor
      Value[d7miss_sel] = paste0( substring(Value[d7miss_sel],1,2)
                                , "-15-"
                                , substring(Value[d7miss_sel],4,7)
                                )#end paste0
      #---~---


      #---~---
      #   Some data sets provided dates in yymmdd format. Temporarily make the 
      # dates dd/mm/yy so the commands below will work.
      #---~---
      IsAuthor        = AuthorName %in% "Bjorn Robroek"
      LengthSix       = (nchar(Value)         %in% 6L)
      swap_sel        = IsAuthor & LengthSix
      Value[swap_sel] = paste( substring(Value[swap_sel],5,6)
                             , substring(Value[swap_sel],3,4)
                             , substring(Value[swap_sel],1,2)
                             , sep ="-"
                             )#end paste
      #---~---

      #---~---
      #   Some authors provided data with 5 digits, which are likely elapsed days since
      # 1900-01-01. Assume this for the time being.
      #---~---
      IsAuthor         = AuthorName %in% c( "Dennis Baldocchi"
                                          , "Joseph Hawes"
                                          , "Jens Kattge"
                                          , "Jan Pisek"
                                          , "Fritz Schweingruber"
                                          , "Mateus Silva"
                                          , "Nick Smith"
                                          , "Christian Wirth"
                                          )#end c
      LengthFive       = ( nchar(Value) %in% 5L )
      HasDash          = grepl(pattern="\\-",x=Value)
      IsElapsed        = IsAuthor & LengthFive & ( ! HasDash )
      Zero             = lubridate::date("1900-01-01")
      suppressWarnings({
         Value[IsElapsed] = as.character(Zero + as.integer(Value[IsElapsed]))
      })#end suppressWarnings
      #--- For now, in case the values have five numbers but author is not listed, crash it.
      bad_sel          = LengthFive & ( (! IsAuthor) | HasDash )
      Value[bad_sel]   = NA_character_
      #Valid[bad_sel]  = FALSE          # We skip this so we spot other cases.
      #VName[bad_sel]  = NA_character_  # We skip this so we spot other cases.
      #---~---


      #--- Some dates have long numbers with many decimals, which are not obvious dates.
      IsAuthor       = AuthorName %in% "Fritz Schweingruber"
      bad_sel        = IsAuthor & ( nchar(Value) %gt% c(10L) )
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #--- Some dates have a non-sensical number of characters. Discard them.
      bad_sel        = nchar(Value) %in% c(1L,2L)
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---

      #--- Fix dates without zeroes.
      has_dash        = grepl(pattern="\\-",x=Value)
      Value[has_dash] = Fix_TRY_DateZero(x=Value[has_dash])
      #---~---


      #---~---
      #   Some data sets have a mix of mm/dd/yyyy and dd/mm/yyyy, or became a mix after
      # standardisation.  We try our best to retain as much information as possible, using
      # the original reference, original values, or additional information retrieved from
      # the metadata.
      #---~---
      #---~---
      #   Joseph Hawes. Mix of date formats. Most of them are in dd-mm-yyyy but some dates 
      # are zero.
      #---~---
      IsAuthor        = AuthorName %in% "Joseph Hawes"
      LengthTen       = ( ( nchar(Value)         %in% 10L )
                        & ( substring(Value,3,3) %in% "-" )
                        & ( substring(Value,6,6) %in% "-" )
                        )#end LengthTen
      swap_sel        = IsAuthor & LengthTen
      DaySwap         = substring(Value[swap_sel], 1, 2)
      MonthSwap       = substring(Value[swap_sel], 4, 5)
      YearSwap        = substring(Value[swap_sel], 7,10)
      DaySwap         = ifelse( test = DaySwap   %in% "00", yes = "15", no = DaySwap )
      DateSwap        = ifelse( test = MonthSwap %in% "00"
                              , yes  = NA_character_
                              , no   = paste(YearSwap,MonthSwap,DaySwap,sep="-")
                              )#end ifelse
      Value[swap_sel] = DateSwap
      Valid[swap_sel] = ! is.na(DateSwap)
      VName[swap_sel] = ifelse( test = is.na(DateSwap)
                              , yes  = NA_character_
                              , no   = VName[swap_sel]
                              )#end ifelse
      #---~---
      #   Karen Holl
      #---~---
      IsAuthor        = AuthorName %in% c( "Karen Holl" )
      swap_sel        = IsAuthor & ( AncilOrig %in% c("1 July; 2014") )
      Value[swap_sel] = paste( substring(Value[swap_sel], 4, 5)
                             , substring(Value[swap_sel], 1, 2)
                             , substring(Value[swap_sel], 7,10)
                             , sep = "-"
                             )#end paste0
      #---~---
      #   Yan-Shih Lin. Data are truly mixed, using the best associations we can infer...
      #---~---
      IsAuthor  = AuthorName %in% c( "Yan-Shih Lin" )
      MonthOrig = IsAuthor & FALSE
      for (m in sequence(12L)){
         MonthOrig = ( MonthOrig 
                     | grepl(pattern=month.name[m],x=AncilOrig,ignore.case=TRUE)
                     | grepl(pattern=month.abb [m],x=AncilOrig,ignore.case=TRUE)
                     )#end MonthOrig
      }#end for (m in sequence(12L))
      HasTime   = grepl(pattern=":"  ,x=AncilOrig,ignore.case=TRUE)
      HasDot    = grepl(pattern="\\.",x=AncilOrig,ignore.case=TRUE)
      LengthTen = ( ( nchar(Value)         %in% 10L )
                  & ( substring(Value,3,3) %in% "-" )
                  & ( substring(Value,6,6) %in% "-" )
                  )#end LengthTen
      suppressWarnings({
         YearLast  = LengthTen & ( as.numeric(substring(Value,7,10)) %gt% 1800L)
      })#end suppressWarnings
      Is2018    = LengthTen & ( substring(Value, 7,10) %in% c("2018") )
      #--- The following are inferred from impossible dates, but some are ambiguous.
      Force2001 = Value  %in% c("06-06-2001","06-08-2001","06-09-2001","07-08-2001"
                               ,"07-19-2001","07-20-2001","07-21-2001","07-22-2001"
                               ,"07-23-2001","08-10-2001","09-10-2001","09-11-2001")
      Force2008 = Value  %in% c("09-02-2008","09-03-2008","09-05-2008","09-06-2008"
                               ,"09-07-2008","09-08-2008","09-09-2008","09-12-2008"
                               ,"09-13-2008","09-15-2008")
      Force2010 = Value  %in% c("06-01-2010","06-03-2010","06-06-2010","06-10-2010"
                               ,"06-28-2010","06-29-2010","06-30-2010","07-06-2010"
                               ,"07-10-2010","08-02-2010","08-04-2010","08-05-2010"
                               ,"08-06-2010","08-10-2010","08-30-2010","09-01-2010"
                               ,"09-10-2010","10-02-2010","10-04-2010","10-05-2010"
                               ,"10-06-2010","10-07-2010")
      Force2011 = Value  %in% c("04-18-2011","04-19-2011","04-20-2011","05-12-2011"
                               ,"05-16-2011","05-18-2011","06-11-2011","06-20-2011"
                               ,"07-18-2011","07-20-2011","08-22-2011","08-24-2011"
                               ,"09-11-2011","09-12-2011","10-03-2011","10-05-2011"
                               ,"10-11-2011")
      #--- Swap characters
      swap_sel  = ( IsAuthor 
                  & LengthTen 
                  & (MonthOrig | Is2018 | Force2001 | Force2008 | Force2010 | Force2011)
                  )#end swap_sel
      Value[swap_sel]   = paste( substring(Value[swap_sel], 4, 5)
                               , substring(Value[swap_sel], 1, 2)
                               , substring(Value[swap_sel], 7,10)
                               , sep = "-"
                               )#end paste0
      #---~---
      #   Dushan Kumarathunge. Original data had a mix of formats. Most of the data are in
      # dd-mm-yyyy, but a few are in other formats. We manually replace the non-standard
      # ones so everything is dd-mm-yyyy.
      #---~---
      IsAuthor        = AuthorName %in% c( "Dushan Kumarathunge" )
      Value[IsAuthor & ( AncilOrig %in% "Jun-sep-2009"  )] = "31-07-2009"
      Value[IsAuthor & ( AncilOrig %in% "Apr-May/1999"  )] = "01-05-1999"
      Value[IsAuthor & ( AncilOrig %in% "July-Aug/2011" )] = "31-07-2011"
      Value[IsAuthor & ( AncilOrig %in% "08/22/1995"    )] = "22-08-1995"
      #---~---
      #   Justin Luong. Original data had a mix of formats, and we swapped some of them to
      # be dd-mm-yyyy. Swap data originally provided in mm/dd/yyyy to the same format.
      #---~---
      IsAuthor        = AuthorName %in% c( "Justin Luong" )
      swap_sel        = IsAuthor & ( AncilOrig %in% c("04/01/2019") )
      Value[swap_sel] = paste( substring(Value[swap_sel], 4, 5)
                             , substring(Value[swap_sel], 1, 2)
                             , substring(Value[swap_sel], 7,10)
                             , sep = "-"
                             )#end paste0
      #---~---
      #   Arthur Vinicius Rodrigues
      #---~---
      IsAuthor        = AuthorName %in% c( "Arthur Vinicius Rodrigues" )
      swap_sel        = IsAuthor & ( Value %in% c("09-20-2017") )
      Value[swap_sel] = paste( substring(Value[swap_sel], 4, 5)
                             , substring(Value[swap_sel], 1, 2)
                             , substring(Value[swap_sel], 7,10)
                             , sep = "-"
                             )#end paste0
      #---~---
      #   Fritz Schweingruber
      #---~---
      IsAuthor        = AuthorName %in% c( "Fritz Schweingruber" )
      swap_sel        = IsAuthor & ( Value %in% c("23-05-2011") )
      Value[swap_sel] = paste( substring(Value[swap_sel], 4, 5)
                             , substring(Value[swap_sel], 1, 2)
                             , substring(Value[swap_sel], 7,10)
                             , sep = "-"
                             )#end paste0
      #---~---
      #   Nikolai Yankov. Original data had a mix of formats, and we appended dates for
      # those containing only mm-yyyy. Swap the filled data so everything is in mm-dd-yyyy
      #---~---
      IsAuthor        = AuthorName %in% c( "Nikolai Yankov" )
      swap_sel        = IsAuthor & ( nchar(AncilOrig) %eq% 7L )
      Value[swap_sel] = paste( substring(Value[swap_sel], 4, 5)
                             , substring(Value[swap_sel], 1, 2)
                             , substring(Value[swap_sel], 7,10)
                             , sep = "-"
                             )#end paste0
      #---~---




      #--- Flag valid entries
      fine_sel = ! is.na(Value)
      #---~---


      #--- Define the main 
      y2_sl_last  = ( fine_sel
                    & ( nchar(Value) %in% 8 )
                    & ( substring(Value,3,3) %in% "-" ) & ( substring(Value,6,6) %in% "-" )
                    )#end year4_last
      y4_sl_last  = ( fine_sel
                    & ( nchar(Value) %in% 10 )
                    & ( substring(Value,3,3) %in% "-" ) & ( substring(Value,6,6) %in% "-" )
                    )#end year4_last
      y4_sl_first = ( fine_sel
                    & ( nchar(Value) %in% 10 )
                    & ( substring(Value,5,5) %in% "-" ) & ( substring(Value,8,8) %in% "-" )
                    )#end year4_last
      y4_ns_first = ( fine_sel
                    & ( nchar(Value) %in% 8 )
                    & ( ! grepl(pattern="\\-",x=Value) )
                    )#end year4_last
      #---~---


      #--- Make sure all data have the same standard date
      dmy2_sl_sel = ( y2_sl_last
                    & all( as.integer(substring(Value[y2_sl_last], 1, 2)) %in% sequence(31L) )
                    & all( as.integer(substring(Value[y2_sl_last], 4, 5)) %in% sequence(12L) )
                    )#end dmy2_sl_sel
      dmy4_sl_sel = ( y4_sl_last
                    & all( as.integer(substring(Value[y4_sl_last], 1, 2)) %in% sequence(31L) )
                    & all( as.integer(substring(Value[y4_sl_last], 4, 5)) %in% sequence(12L) )
                    & all( as.integer(substring(Value[y4_sl_last], 7,10)) %gt% 1700L)
                    )#end dmy4_sl_sel
      mdy2_sl_sel = ( y2_sl_last
                    & all( as.integer(substring(Value[y2_sl_last], 1, 2)) %in% sequence(12L) )
                    & all( as.integer(substring(Value[y2_sl_last], 4, 5)) %in% sequence(31L) )
                    )#end dmy2_sl_sel
      mdy4_sl_sel = ( y4_sl_last
                    & all( as.integer(substring(Value[y4_sl_last], 1, 2)) %in% sequence(12L) )
                    & all( as.integer(substring(Value[y4_sl_last], 4, 5)) %in% sequence(31L) )
                    & all( as.integer(substring(Value[y4_sl_last], 7,10)) %gt% 1700L )
                    )#end mdy4_sl_sel
      y4md_sl_sel = ( y4_sl_first
                    & all( as.integer(substring(Value[y4_sl_first], 1, 4)) %gt% 1700L )
                    & all( as.integer(substring(Value[y4_sl_first], 6, 7)) %in% sequence(12L) )
                    & all( as.integer(substring(Value[y4_sl_first], 9,10)) %in% sequence(31L) )
                    )#end y4md_sl_sel
      y4md_ns_sel = ( y4_ns_first
                    & all( as.integer(substring(Value[y4_ns_first],1,4)) %gt% 1700L )
                    & all( as.integer(substring(Value[y4_ns_first],5,6)) %in% sequence(12L) )
                    & all( as.integer(substring(Value[y4_ns_first],7,8)) %in% sequence(31L) )
                    )#end y4md_ns_sel
      year4_sel   = fine_sel & ( nchar(Value) %in% 4L)
      #---~---



      #---~---
      #   Some data sets can be ambiguous if all data were collected early in the month.
      # In these cases we impose one of them based on closer data inspection (which may
      # be uncertain).
      #---~---
      #--- Keep mm-dd-yyyy
      IsAuthor              = AuthorName %in% c( "Mehdi Abedi"
                                               , "Deborah Apgaua"
                                               , "Michael Belluau"
                                               , "Federico Brumnich"
                                               , "Chaeho Byun"
                                               , "Fatih Fazlioglu"
                                               , "Jane Catford"
                                               , "Gregoire Freschet"
                                               , "Tucker Gilman"
                                               , "Andres Gonzalez-Melo"
                                               , "Melanie Harze"
                                               , "Karen Holl"
                                               , "Marney Isaac"
                                               , "Gianalberto Losapio"
                                               , "Jens Kattge"
                                               , "Tamir Klein"
                                               , "Zia Mehrabi"
                                               , "Sean Michaletz"
                                               , "Chrysanthi Michelaki"
                                               , "Begona Peco"
                                               , "Lourens Poorter"
                                               , "Fiona Soper"
                                               , "Tsvetelina Terziyska"
                                               , "Jose M. Torres-Ruiz"
                                               , "Alexia Totte"
                                               , "Harry Watkins"
                                               , "Benjamin Yguel"
                                               , "Nikolai Yankov"
                                               )#end c
      dmy2_sl_sel[IsAuthor] = FALSE
      dmy4_sl_sel[IsAuthor] = FALSE
      #--- Keep dd-mm-yyyy
      IsAuthor              = AuthorName %in% c( "Michele Carbognani"
                                               , "Han Chen"
                                               , "Greg Guerin"
                                               , "Nate Hough-Snee"
                                               , "Dushan Kumarathunge"
                                               , "Daniel Laughlin"
                                               , "Justin Luong"
                                               , "Benjamin Jackson"
                                               , "Kevin Karbstein"
                                               , "Vamsi Krishna Kommineni"
                                               , "Ian McFadden"
                                               , "Christian Rossi"
                                               , "Rob Salguero-Gomez"
                                               , "Enio Sosinski"
                                               )#end c
      mdy2_sl_sel[IsAuthor] = FALSE
      mdy4_sl_sel[IsAuthor] = FALSE
      #---~---

      #---~---
      #   Set day
      #---~---
      ValueDay              = rep(NA_integer_,times=length(Value))
      ValueDay[dmy2_sl_sel] = as.integer(substring(Value[dmy2_sl_sel], 1, 2))
      ValueDay[dmy4_sl_sel] = as.integer(substring(Value[dmy4_sl_sel], 1, 2))
      ValueDay[mdy2_sl_sel] = as.integer(substring(Value[mdy2_sl_sel], 4, 5))
      ValueDay[mdy4_sl_sel] = as.integer(substring(Value[mdy4_sl_sel], 4, 5))
      ValueDay[y4md_sl_sel] = as.integer(substring(Value[y4md_sl_sel], 9,10))
      ValueDay[y4md_ns_sel] = as.integer(substring(Value[y4md_ns_sel], 7, 8))
      #---~---


      #---~---
      #   Set month
      #---~---
      ValueMonth              = rep(NA_integer_,times=length(Value))
      ValueMonth[dmy2_sl_sel] = as.integer(substring(Value[dmy2_sl_sel], 4, 5))
      ValueMonth[dmy4_sl_sel] = as.integer(substring(Value[dmy4_sl_sel], 4, 5))
      ValueMonth[mdy2_sl_sel] = as.integer(substring(Value[mdy2_sl_sel], 1, 2))
      ValueMonth[mdy4_sl_sel] = as.integer(substring(Value[mdy4_sl_sel], 1, 2))
      ValueMonth[y4md_sl_sel] = as.integer(substring(Value[y4md_sl_sel], 6, 7))
      ValueMonth[y4md_ns_sel] = as.integer(substring(Value[y4md_ns_sel], 5, 6))
      #---~---


      #---~---
      #   Set year
      #---~---
      ValueYear              = rep(NA_integer_,times=length(Value))
      ValueYear[dmy2_sl_sel] = as.integer(substring(Value[dmy2_sl_sel], 7, 8))
      ValueYear[dmy4_sl_sel] = as.integer(substring(Value[dmy4_sl_sel], 7,10))
      ValueYear[mdy2_sl_sel] = as.integer(substring(Value[mdy2_sl_sel], 7, 8))
      ValueYear[mdy4_sl_sel] = as.integer(substring(Value[mdy4_sl_sel], 7,10))
      ValueYear[y4md_sl_sel] = as.integer(substring(Value[y4md_sl_sel], 1, 4))
      ValueYear[y4md_ns_sel] = as.integer(substring(Value[y4md_ns_sel], 1, 4))
      #---~---


      #---~---
      #   For entries with 2-digit years, assume current century if year is less than or
      # equal to the last two digits of the current year, or previous century otherwise.
      #---~---
      Year4Now             = as.integer(substring(Sys.time(),1,4))
      Year2Now             = Year4Now %% 100L
      CenturyNow           = 100L * floor(Year4Now / 100L)
      CenturyPrev          = CenturyNow - 100L
      year2_sel            = dmy2_sl_sel | mdy2_sl_sel
      ValueYear[year2_sel] = ifelse( test = ValueYear[year2_sel] %le% Year2Now
                                   , yes  = CenturyNow  + ValueYear[year2_sel]
                                   , no   = CenturyPrev + ValueYear[year2_sel]
                                   )#end ifelse
      #---~---

      #---~---
      #   Ignore dates when only year is provided
      #---~---
      Value[year4_sel] = NA_character_
      Valid[year4_sel] = FALSE
      VName[year4_sel] = NA_character_
      #---~---



      #---~---
      #   Check for ambiguous cases
      #---~---
      IsAmbiguous = ( (dmy2_sl_sel & mdy2_sl_sel) | (dmy4_sl_sel & mdy4_sl_sel) )
      if (any(IsAmbiguous)){
         cat0(" ----- Check ambiguous dates...")
         browser()
      }#end if (any(IsAmbiguous))
      # Mess = Valid & ( is.na(ValueDay) | is.na(ValueMonth) | is.na(ValueYear) )
      # if (any(Mess)) browser()
      #---~---

      #---~---
      #   Concatenate date in standard format (yyyy-mm-dd)
      #---~---
      # if (any(IsAuthor) & (DataID %in% 6601L)) browser()
      Value = ifelse( test = is.finite(ValueDay  ) & is.finite(ValueMonth)
                           & is.finite(ValueYear )
                    , yes  = sprintf("%4.4i-%2.2i-%2.2i",ValueYear,ValueMonth,ValueDay)
                    , no   = NA_character_
                    )#end ifelse
      #---~---

      #--- Delete data without units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! ( dmy2_sl_sel | dmy4_sl_sel | mdy2_sl_sel | mdy4_sl_sel | y4md_sl_sel 
                            | y4md_ns_sel | year4_sel   )
      Value[unmiss_sel] = NA_character_
      #---~---

   }else if (AncilTime){
      #---~---
      #   Mean length of dry season
      #   Plant age at measurement
      #   Plant developmental status / plant age / maturity / plant life stage
      #   Stand age
      #   Stand age class
      #   Time since fire
      #---~---


      #---~---
      #   Some age data have values but no units. They are likely years so we assume years.
      #---~---
      IsUnitMiss = is.na(UnitOrig)
      IsDrySeason = DataID %in% c(976L)
      IsPlantAge  = DataID %in% c(1414L,1832L)
      IsPlantDev  = DataID %in% c(413L)
      IsStandCls  = DataID %in% c(3031L)
      IsStandAge  = DataID %in% c(4696L)
      IsTimeFire  = DataID %in% c(3885L)
      AssumeYear  = ( IsUnitMiss
                    & ( ( IsTimeFire & (AuthorName %in% "Isabelle Aubin"  ) )
                      | ( IsPlantDev & (AuthorName %in% "Maxime Cailleret") )
                      | ( IsPlantDev & (AuthorName %in% "Herve Jactel"    ) )
                      | ( IsPlantAge & (AuthorName %in% "Eric Lamb"       ) )
                      | ( IsPlantAge & (AuthorName %in% "Christian Wirth" ) )

                      )#end AuthorName
                    )#end UnitMiss
      UnitOrig[AssumeYear] = "year"
      #---~---



      #---~---
      #   Some authors provided information with text. Manually fix the information.
      #---~---
      #--- Coline Boonman
      IsAuthor              = AuthorName %in% c("Coline Boonman")
      AssumeMonth           = IsAuthor & IsPlantDev & ( Value %in% "seedlings.6monthold" )
      Value   [AssumeMonth] = "6"
      UnitOrig[AssumeMonth] = "month"
      #--- Peter Manning
      IsAuthor              = AuthorName %in% c("Peter Manning")
      AssumeWeek            = IsAuthor & IsPlantDev & ( Value %in% "11weeks-peakbiomass" )
      Value   [AssumeWeek ] = "11"
      UnitOrig[AssumeWeek ] = "week"
      #--- Giacomo Puglielli
      IsAuthor              = AuthorName %in% c("Giacomo Puglielli")
      AssumeYear            = IsAuthor & IsPlantDev & ( Value %in% "?3years" )
      Value   [AssumeYear ] = "3"
      UnitOrig[AssumeYear ] = "year"
      #---~---



      #---~---
      #   Some authors did not provide units but data are likely in years.
      #---~---
      IsAuthor             = AuthorName %in% c("Enqing Hou")
      AssumeYear           = IsAuthor & IsStandAge
      UnitOrig[AssumeYear] = "year"
      #---~---



      #---~---
      #   Some authors listed units in the original name, not in the units.
      #---~---
      IsAuthor              = AuthorName %in% c("Tarin Toledo-Aceves")
      AssumeMonth           = 
         IsAuthor & IsPlantAge & ( NameOrig %in% "individual age (months)" )
      UnitOrig[AssumeMonth] = "month"
      #---~---



      #---~---
      #   Some authors did not provide units but data are likely in years.
      #---~---
      IsAuthor             = AuthorName %in% c("Daijun Liu")
      AssumeYear           = IsAuthor & IsPlantDev
      UnitOrig[AssumeYear] = "year"
      #---~---



      #---~---
      #   Some data have too little information to unambiguously identify units.
      #---~---
      IsAuthor       = AuthorName %in% c( "Julie Messier")
      bad_sel        = IsAuthor & IsPlantDev
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---



      #---~---
      #   Some authors provided reproductive stage as opposed to plant age to maturity.
      #---~---
      IsAuthor       = AuthorName %in% c( "Michael J. Aspinwall", "Mason Heberling"
                                        , "Enqing Hou", "Ian McFadden"
                                        , "Moises Mendez Toribio", "Jose M. Torres-Ruiz"
                                        , "Mark van Kleunen" )
      bad_sel        = IsAuthor & IsPlantDev
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---



      #---~---
      #   Some authors provided reproductive stage as opposed to age.
      #---~---
      IsAuthor       = AuthorName %in% c( "Tamir Klein"
                                        , "Michael Scherer-Lorenzen"
                                        )#end c
      bad_sel        = IsAuthor & IsPlantAge
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---




      #---~---
      #   Some data have stand age instead of actual plant life state.
      #---~---
      IsAuthor       = AuthorName %in% c( "Jan Pisek"
                                        , "Lourens Poorter"
                                        , "Bill Shipley"
                                        , "Kris Verheyen"
                                        , "Christian Wirth"
                                        )#end c
      bad_sel        = IsAuthor & IsPlantDev
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #--- Remove spurious characters (but track those with units in the value).
      has_day = ( grepl(pattern="d$"     ,x=Value)
                | grepl(pattern="day.$"  ,x=Value) )
      has_wk  = ( grepl(pattern="w$"     ,x=Value)
                | grepl(pattern="wk$"    ,x=Value)
                | grepl(pattern="wk.$"   ,x=Value)
                | grepl(pattern="week$"  ,x=Value)
                | grepl(pattern="week.$" ,x=Value) )
      has_mon = ( grepl(pattern="m$"     ,x=Value)
                | grepl(pattern="mon$"   ,x=Value)
                | grepl(pattern="mon.$"  ,x=Value)
                | grepl(pattern="month$" ,x=Value)
                | grepl(pattern="month.$",x=Value) )
      has_yr  = ( grepl(pattern="y$"     ,x=Value)
                | grepl(pattern="yr$"    ,x=Value)
                | grepl(pattern="yr.$"   ,x=Value)
                | grepl(pattern="year$"  ,x=Value)
                | grepl(pattern="year.$" ,x=Value)
                | grepl(pattern="Year.$" ,x=Value)
                | grepl(pattern="years.$",x=Value)
                | grepl(pattern="Years.$",x=Value) )
      Value   = gsub(pattern=" "      ,replacement="",x=Value)
      Value   = gsub(pattern="d$"     ,replacement="",x=Value)
      Value   = gsub(pattern="day.$"  ,replacement="",x=Value)
      Value   = gsub(pattern="w$"     ,replacement="",x=Value)
      Value   = gsub(pattern="wk$"    ,replacement="",x=Value)
      Value   = gsub(pattern="wk.$"   ,replacement="",x=Value)
      Value   = gsub(pattern="week$"  ,replacement="",x=Value)
      Value   = gsub(pattern="week.$" ,replacement="",x=Value)
      Value   = gsub(pattern="m$"     ,replacement="",x=Value)
      Value   = gsub(pattern="mon$"   ,replacement="",x=Value)
      Value   = gsub(pattern="mon.$"  ,replacement="",x=Value)
      Value   = gsub(pattern="month$" ,replacement="",x=Value)
      Value   = gsub(pattern="month.$",replacement="",x=Value)
      Value   = gsub(pattern="y$"     ,replacement="",x=Value)
      Value   = gsub(pattern="yr$"    ,replacement="",x=Value)
      Value   = gsub(pattern="yr.$"   ,replacement="",x=Value)
      Value   = gsub(pattern="year$"  ,replacement="",x=Value)
      Value   = gsub(pattern="year.$" ,replacement="",x=Value)
      Value   = gsub(pattern="Year.$" ,replacement="",x=Value)
      Value   = gsub(pattern="years.$",replacement="",x=Value)
      Value   = gsub(pattern="Years.$",replacement="",x=Value)
      #---~---


      #---~---
      #   Prevent script to continuing in case there are characters.
      #---~---
      HasLetter = grepl(pattern="[a-z]",x=Value,ignore.case=FALSE)
      if (any(HasLetter)){
         cat0(" ----- Check potentially dangerous variable...")
         browser()
      }#end if (any(HasLetter))
      #---~---


      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---

      #--- Make sure all data have the same units (year). Discard percent data
      year_sel         = 
         ( has_yr | ( UnitOrig %in% c( "a", "y", "year", "years", "years *see notes"
                                     , "yr", "yrs", "Year", "Years" ) ) )
      month_sel        = has_mon | ( UnitOrig %in% c("mo","month","months") )
      week_sel         = has_wk  | ( UnitOrig %in% c("week","weeks") )
      day_sel          = has_day | ( UnitOrig %in% c("d","day","days") )
      Value[month_sel] = as.character( 1. / yr.mon  * (as.numeric(Value[month_sel])) )
      Value[week_sel ] = as.character( 1. / yr.week * (as.numeric(Value[week_sel ])) )
      Value[day_sel  ] = as.character( 1. / yr.day  * (as.numeric(Value[day_sel  ])) )
      #---~---


      #--- Delete data without units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! ( year_sel | month_sel | week_sel | day_sel )
      Value[unmiss_sel] = NA_character_
      #---~---

   }else if (AncilRad){
      #---~---
      #   Annual mean radiation
      #   Light during measurement
      #   Radiation during measurement
      #---~---

      #--- Remove spurious characters.
      Value = gsub(pattern="\\?",replacement="",x=Value)
      #---~---

      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---


      #---~---
      #   Some authors reported the incorrect units.
      #---~---
      IsAuthor           = AuthorName %in% "Anthony Walker"
      UnitOrig[IsAuthor] = "umol m-2 s-1"
      #---~---


      #---~---
      #   Some authors used numbers to report missing data.
      #---~---
      IsAuthor           = AuthorName %in% "Anthony Walker"
      IsMissing          = IsAuthor & ( Value %in% "-10000" )
      Value[IsMissing]   = NA_character_
      Valid[IsMissing]   = FALSE
      VName[IsMissing]   = NA_character_
      #---~---

      #---~---
      #   Some authors provided the units in the variable name instead of the unit column.
      #---~---
      IsAuthor           = ( AuthorName %in% "Peter Reich"
                           & NameOrig   %in% "radiation_av(w/m2)" )
      UnitOrig[IsAuthor] = "W m-2"
      #---~---


      #--- Make sure all data have the same units (W/m2).
      umolom2os_sel        = UnitOrig %in% c( "m;icromol/m2/s"
                                            , "mico mol m-1 s-1"
                                            , "micro mol / m2 / s"
                                            , "micro mol m-2 s-1"
                                            , "micro mol m2 s"
                                            , "micro mol m2 s-1"
                                            , "micro mol/m2/s"
                                            , "micro mol/m2/s1"
                                            , "micro mols m-2 s-1"
                                            , "micromol CO2 m-2 leaf area s-1"
                                            , "micromol m-2 s-1"
                                            , "micro mol photons m-2 s-1"
                                            , "micromol/m2/s"
                                            , "micromol/m2/s PAR"
                                            , "micromol/m2/s1 PAR"
                                            , "micromoles/m2/s"
                                            , "micromolm-2s-1"
                                            , "umol m-2 s-1"
                                            , "umol/m2/s"
                                            , "umol photons / m2 / sec"
                                            )#end c
      wom2_sel             = UnitOrig %in% c("W/m2","W m-2")
      Value[umolom2os_sel] = as.character( umol.2.mol * Ein.2.Watts
                                         * (as.numeric(Value[umolom2os_sel]))
                                         )#end as.character
      #---~---


      #--- Discard mV data 
      bad_sel        = UnitOrig %in% c( "mV" )
      Value[bad_sel] = NA_character_
      Valid[bad_sel] = FALSE
      VName[bad_sel] = NA_character_
      #---~---


      #--- Delete data without units (for debugging, we do not set them as invalid).
      unmiss_sel        = ! ( umolom2os_sel | wom2_sel )
      Value[unmiss_sel] = NA_character_
      #---~---

   }else if (AncilHumid){
      #---~---
      #   Relative air humidity during measurement (Relative humidity )
      #---~---


      #--- Remove spurious characters (but track those with units in the value).
      has_pc = ( grepl(pattern="\\%$"     ,x=Value) )
      Value   = gsub(pattern=" "      ,replacement="",x=Value)
      Value   = gsub(pattern="\\%$"   ,replacement="",x=Value)
      #---~---


      #--- Turn inequality and range values into numbers (albeit highly uncertain).
      Value = TRY_Fix_Uncertain_Str(Value)
      #---~---

      #--- Make sure all data have the same units (%).
      pc_sel = has_pc | ( UnitOrig %in% c("%","%RH","percent") )
      #---~---


      #--- Delete data without units (for debugging, we do not set them as invalid).
      Value[! pc_sel] = NA_character_
      #---~---

   }else if (AncilCountry){
      #---~---
      #   Location Country
      #---~---


      #--- Discard invalid data
      IsInvalid        = UnitOrig %in% c( "mV" )
      Value[IsInvalid] = NA_character_
      Valid[IsInvalid] = FALSE
      VName[IsInvalid] = NA_character_
      #---~---


      #---~---
      #   Here we fix the name of countries that were not correctly spelt or have changed
      # names.  Some authors included sub-national information instead of the actual country
      # name.  If the country is very large (>= Australia) or if the country has regions of
      # very different biomes across continents (e.g., France, Denmark), we retain the
      # sub-national information.
      #---~---
      #--- Argentina
      Value[Value %in% "Argentian"                   ] = "Argentina"
      #--- Australia: Keep states and territories if available
      Value[Value %in% "Austrtalia"                  ] = "Australia"
      Value[Value %in% "Australia-ACT"               ] = "Australia ACT"
      Value[Value %in% "Australia-FNQ"               ] = "Australia QLD"
      Value[Value %in% "Australia-NSW"               ] = "Australia NSW"
      Value[Value %in% "Australia-SA"                ] = "Australia SA"
      Value[Value %in% "Australia-TAS"               ] = "Australia TAS"
      Value[Value %in% "Australia-WA"                ] = "Australia WA"
      #--- Austria
      Value[Value %in% "Alpen"                       ] = "Austria" # Not enough information
      Value[Value %in% "Alps"                        ] = "Austria" # Not enough information
      Value[Value %in% "Austira"                     ] = "Austria" # Not enough information
      #--- Azerbaijan
      Value[Value %in% "Azerbajdzan"                 ] = "Azerbaijan"
      #--- Brazil: Keep states if available
      Value[Value %in% "Brasil"                      ] = "Brazil"
      Value[Value %in% "Brazil-Amazon"               ] = "Brazil AM" # Based on lon/lat
      #--- Bulgaria
      Value[Value %in% "Bulgarien"                   ] = "Bulgaria"
      #--- Burkina Faso
      Value[Value %in% "Burkino Faso"                ] = "Burkina Faso"
      #--- Cameroon
      Value[Value %in% "Kamerun"                     ] = "Cameroon"
      #--- Canada: Keep provinces if available
      Value[Value %in% "Alberta"                     ] = "Canada AB"
      Value[Value %in% "British Columbia"            ] = "Canada BC"
      Value[Value %in% "Manitoba"                    ] = "Canada MB"
      #--- Canary Islands (Spanish Macaronesia)
      Value[Value %in% "Macaronesia; Spain"          ] = "Canary Islands"
      Value[Value %in% "Macaronesia; Tenerife"       ] = "Canary Islands"
      Value[Value %in% "Macaronesia;Spain"           ] = "Canary Islands"
      #--- Congo (Brazzaville) (aka ROC)
      Value[Value %in% "Republic of Congo"           ] = "Congo (Brazzaville)"
      Value[Value %in% "Republic of the Congo"       ] = "Congo (Brazzaville)"
      #--- Congo (Kinshasa) (aka DRC)
      Value[Value %in% "Democratic Republic of Congo"] = "Congo (Kinshasa)"
      #--- Costa Rica
      Value[Value %in% "Costa Rica Limon"            ] = "Costa Rica"
      Value[Value %in% "Costa_Rica"                  ] = "Costa Rica"
      Value[Value %in% "Northwest Costa Rica:"       ] = "Costa Rica"
      #--- Czechia (Czech Republic)
      Value[Value %in% "Czech Republic"              ] = "Czechia"
      Value[Value %in% "czechia"                     ] = "Czechia"
      #--- Denmark
      Value[Value %in% "Demark"                      ] = "Denmark"
      #--- Ethiopia
      Value[Value %in% "Abessynia"                   ] = "Ethiopia"
      #--- Egypt
      Value[Value %in% "Aegypt"                      ] = "Egypt"
      #--- France (likely in Europe)
      Value[Value %in% "FRANCE"                      ] = "France"
      Value[Value %in% "Frankreich"                  ] = "France"
      Value[Value %in% "Freance"                     ] = "France"
      #--- French Guiana (overseas region of France)
      Value[Value %in% "Cayenne"                     ] = "French Guiana"
      Value[Value %in% "French Guia"                 ] = "French Guiana"
      Value[Value %in% "Guyane Francaise"            ] = "French Guiana"
      #--- Germany
      Value[Value %in% "Deutschland"                 ] = "Germany"
      Value[Value %in% "Gemany"                      ] = "Germany"
      Value[Value %in% "GERMANY"                     ] = "Germany"
      Value[Value %in% "Germeny"                     ] = "Germany"
      #--- Greenland (Denmark)
      Value[Value %in% "Grenland"                    ] = "Greenland"
      Value[Value %in% "groenland"                   ] = "Greenland"
      Value[Value %in% "Groenland"                   ] = "Greenland"
      Value[Value %in% "Gronland"                    ] = "Greenland"
      #--- Guyana
      Value[Value %in% "Guyanas"                     ] = "Guyana"
      Value[Value %in% "Gyana"                       ] = "Guyana"
      #--- Hungary
      Value[Value %in% "Hungaria"                    ] = "Hungary"
      #--- Kyrgyzstan
      Value[Value %in% "Kyrgiztan"                   ] = "Kyrgyzstan"
      #--- Iceland
      Value[Value %in% "Icland"                      ] = "Iceland"
      #--- Indonesia
      Value[Value %in% "Bawean Island"               ] = "Indonesia"
      Value[Value %in% "Irian Jaya"                  ] = "Indonesia"
      Value[Value %in% "Java"                        ] = "Indonesia"
      Value[Value %in% "Kalimantan"                  ] = "Indonesia"
      Value[Value %in% "Moluccas"                    ] = "Indonesia"
      Value[Value %in% "Seram"                       ] = "Indonesia"
      Value[Value %in% "Sulawesi"                    ] = "Indonesia"
      Value[Value %in% "Sumatra"                     ] = "Indonesia"
      #--- Italy
      Value[Value %in% "Italien"                     ] = "Italy"
      #--- Ivory Coast
      Value[Value %in% "Elfenbeinkuste"              ] = "Ivory Coast"
      #--- Kenya
      Value[Value %in% "Kenia"                       ] = "Kenya"
      Value[Value %in% "Kenya_Uganda"                ] = "Kenya" # Not possible to separate.
      #---- Lithuania
      Value[Value %in% "Lituania"                    ] = "Lithuania"
      #---- Madeira Island (Portuguese Macaronesia)
      Value[Value %in% "Macaronesia Portugal"        ] = "Madeira Island"
      Value[Value %in% "Macaronesia; Portugal"       ] = "Madeira Island"
      Value[Value %in% "Macaronesia;Portugal"        ] = "Madeira Island"
      #--- Malaysia
      Value[Value %in% "Borneo"                      ] = "Malaysia" # Based on lon/lat
      Value[Value %in% "Malay"                       ] = "Malaysia"
      Value[Value %in% "Malay Islands"               ] = "Malaysia"
      Value[Value %in% "Malay Peninsula"             ] = "Malaysia"
      Value[Value %in% "Malaysia-Borneo"             ] = "Malaysia"
      Value[Value %in% "Sarawak"                     ] = "Malaysia"
      #--- Mexico
      Value[Value %in% "Mexiko"                      ] = "Mexico"
      #--- Morocco
      Value[Value %in% "Marocco"                     ] = "Morocco"
      #--- Netherlands
      Value[Value %in% "Holland"                     ] = "Netherlands"
      Value[Value %in% "Netharland"                  ] = "Netherlands"
      Value[Value %in% "Netherland"                  ] = "Netherlands"
      Value[Value %in% "The Netherland"              ] = "Netherlands"
      Value[Value %in% "The Netherlands"             ] = "Netherlands"
      #--- Norway
      Value[Value %in% "NORWAY"                      ] = "Norway"
      #--- Panama
      Value[Value %in% "Pama"                        ] = "Panama"
      #--- Peru
      Value[Value %in% "Peru-Amazon"                 ] = "Peru"
      Value[Value %in% "Peru-Andes"                  ] = "Peru"
      #--- Philippines
      Value[Value %in% "Phillipines"                 ] = "Philippines"
      #--- Portugal
      Value[Value %in% "Protugal"                    ] = "Portugal"
      Value[Value %in% "PRT"                         ] = "Portugal"
      #--- Puerto Rico (US territory)
      Value[Value %in% "USA (Puerto Rico)"           ] = "Puerto Rico"
      #--- Russia: Keep subnational indicators if available
      Value[Value %in% "far east of Russia"          ] = "Russia SA"  # Based on description
      Value[Value %in% "Rusiia"                      ] = "Russia"
      Value[Value %in% "russia"                      ] = "Russia IRK" # Based on lon/lat
      Value[Value %in% "Russia-Siberia"              ] = "Russia SA"  # Based on lon/lat
      #--- Saint Lucia
      Value[Value %in% "St. Lucia"                   ] = "Saint Lucia"
      #--- Serbia
      Value[Value %in% "Ex Jugoslavia"               ] = "Serbia" # Based on lon/lat (though most points would fall in Greece)
      #--- Slovenia
      Value[Value %in% "Slowenien"                   ] = "Slovenia"
      #--- South Africa
      Value[Value %in% "S.Africa"                    ] = "South Africa"
      Value[Value %in% "SAfrica"                     ] = "South Africa"
      Value[Value %in% "Sth Africa"                  ] = "South Africa"
      #--- South Georgia and the South Sandwich Islands
      Value[Value %in% "S Georgia Island"            ] = "South Georgia and the South Sandwich Islands"
      #--- South Korea
      Value[Value %in% "Korea"                       ] = "South Korea" # Based on partial lon/lat
      #--- Spain
      Value[Value %in% "ESP"                         ] = "Spain"       # Based on the author
      Value[Value %in% "spain"                       ] = "Spain"
      Value[Value %in% "Spain"                       ] = "Spain"
      Value[Value %in% "Spain-Guadalajara"           ] = "Spain"
      Value[Value %in% "Spanien"                     ] = "Spain"
      #--- Sri Lanka
      Value[Value %in% "SriLanka"                    ] = "Sri Lanka"
      #--- Suriname
      Value[Value %in% "Surime"                      ] = "Suriname"
      #--- Svalbard (Arctic Norway)
      Value[Value %in% "Norway Svalbard"             ] = "Svalbard"
      #--- Sweden
      Value[Value %in% "South Sweden"                ] = "Sweden"
      Value[Value %in% "Sveden"                      ] = "Sweden"
      Value[Value %in% "SWEDEN"                      ] = "Sweden"
      #--- Switzerland
      Value[Value %in% "Schwitzerland"               ] = "Switzerland"
      Value[Value %in% "Swiss"                       ] = "Switzerland"
      Value[Value %in% "switzerland"                 ] = "Switzerland"
      Value[Value %in% "Switzerland; Alps"           ] = "Switzerland"
      #--- Tanzania
      Value[Value %in% "Tanganyika"                  ] = "Tanzania"
      #--- Trinidad and Tobago
      Value[Value %in% "Trinidad"                    ] = "Trinidad and Tobago"
      Value[Value %in% "Trinidad & Tobago"           ] = "Trinidad and Tobago"
      #--- Tunisia
      Value[Value %in% "Tunesia"                     ] = "Tunisia"
      #--- Turkmenistan
      Value[Value %in% "Turkmenya"                   ] = "Turkmenistan"
      #--- Ukraine
      Value[Value %in% "Ukraina"                     ] = "Ukraine"

      #--- United States.  Keep states if available
      Value[Value %in% "Alaska"                      ] = "United States AK"
      Value[Value %in% "Arizona"                     ] = "United States AZ"
      Value[Value %in% "California"                  ] = "United States CA"
      Value[Value %in% "Connecticut"                 ] = "United States CT"
      Value[Value %in% "Florida"                     ] = "United States FL"
      Value[Value %in% "Kansas"                      ] = "United States KS"
      Value[Value %in% "Massachusetts"               ] = "United States MA"
      Value[Value %in% "Michigan"                    ] = "United States MI"
      Value[Value %in% "Montana"                     ] = "United States MT"
      Value[Value %in% "Nebraska"                    ] = "United States NE"
      Value[Value %in% "New York"                    ] = "United States NY"
      Value[Value %in% "North Carolina"              ] = "United States NC"
      Value[Value %in% "Oregon"                      ] = "United States OR"
      Value[Value %in% "Pennsylvania"                ] = "United States PA"
      Value[Value %in% "Tennessee"                   ] = "United States TN"
      Value[Value %in% "Texas"                       ] = "United States TX"
      Value[Value %in% "US"                          ] = "United States"
      Value[Value %in% "USA"                         ] = "United States"
      Value[Value %in% "USA-AL"                      ] = "United States AL"
      Value[Value %in% "USA-CO"                      ] = "United States CO"
      Value[Value %in% "USA-IW"                      ] = "United States IW"
      Value[Value %in% "USA-MI"                      ] = "United States MI"
      Value[Value %in% "USA-MN"                      ] = "United States MN"
      Value[Value %in% "USA-NC"                      ] = "United States NC"
      Value[Value %in% "USA-NM"                      ] = "United States NM"
      Value[Value %in% "USA-NY"                      ] = "United States NY"
      Value[Value %in% "USA-PN"                      ] = "United States PA"
      Value[Value %in% "USA-SC"                      ] = "United States SC"
      Value[Value %in% "USA-TN"                      ] = "United States TN"
      Value[Value %in% "USA-WI"                      ] = "United States WI"
      Value[Value %in% "Utah"                        ] = "United States UT"
      Value[Value %in% "Virginia"                    ] = "United States VA"
      Value[Value %in% "Washington"                  ] = "United States WA"
      Value[Value %in% "Wisconsin"                   ] = "United States WI"
      #--- United Kingdom
      Value[Value %in% "England"                     ] = "United Kingdom"
      Value[Value %in% "Great britain"               ] = "United Kingdom"
      Value[Value %in% "Great Britain"               ] = "United Kingdom"
      Value[Value %in% "Great Britian"               ] = "United Kingdom"
      Value[Value %in% "Greatm Britain"              ] = "United Kingdom"
      Value[Value %in% "Greatn Britain"              ] = "United Kingdom"
      Value[Value %in% "Gret Britain"                ] = "United Kingdom"
      Value[Value %in% "Scotland"                    ] = "United Kingdom"
      Value[Value %in% "UK"                          ] = "United Kingdom"
      #--- Vanuatu
      Value[Value %in% "New Hebrides (today Vanuatu)"] = "Vanuatu"
      #---~---

      #---~---
      #   The following have some information, but not at country level, and insufficient
      # information (e.g., lon/lat) to infer the country.  List continent instead
      #---~---
      Value[Value %in% "Central Europa"              ] = "Europe"
      Value[Value %in% "Central Europe"              ] = "Europe"
      Value[Value %in% "entral Europe"               ] = "Europe"
      Value[Value %in% "North America"               ] = "North America" # For tracking
      #---~---

      #--- Missing values, or too inaccurate values set 
      IsMissing        = Value %in% c("undefined")
      Value[IsMissing] = NA_character_
      Valid[IsMissing] = FALSE
      VName[IsMissing] = NA_character_
      #---~---

   }else if (AncilContinent){
      #---~---
      #   Location Continent
      #---~---


      #---~---
      #   Make results case insensitive
      #---~---
      Value    = tolower(Value)
      #---~---


      #--- Discard invalid or ambiguous data.
      Discard        = ( (! grepl(pattern="[a-z]",x=Value) )
                       | ( Value %in% c( "australia; s-asia", "n-africa; near east") )
                       )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---



      #---~---
      #   Standardise continent names using the 7-continent approach; i.e., 
      # distinguishing Europe from Asia, South America from North America (and assuming
      # Central America as part of North America).  For simplicity, we group the Pacific
      # Islands with the Australian continent as Oceania.
      #---~---
      IsAfrica       = Value %in% c( "af", "afr", "africa", "macaronesia", "west africa")
      IsAntarctica   = Value %in% c( "antarctica")
      IsAsia         = Value %in% c( "asi", "asia", "east asia")
      IsEurope       = Value %in% c( "e", "eur", "europa; russia;georgia", "europe")
      IsNorthAmerica = Value %in% c( "central america", "greenland", "n-america", "nam"
                                   , "north america"
                                   )#end c
      IsOceania      = Value %in% c( "aus", "australia", "austrtalia", "oceania", "pac")
      IsSouthAmerica = Value %in% c( "s-america", "sam", "south america")
      #---~---



      #---~---
      #   Here we fix the name of countries that were not correctly spelt or have changed
      # names.  Some authors included sub-national information instead of the actual country
      # name.  If the country is very large (>= Australia) or if the country has regions of
      # very different biomes across continents (e.g., France, Denmark), we retain the
      # sub-national information.
      #---~---
      Value[IsAfrica      ] = "Africa"
      Value[IsAntarctica  ] = "Antarctica"
      Value[IsAsia        ] = "Asia"
      Value[IsEurope      ] = "Europe"
      Value[IsNorthAmerica] = "North America"
      Value[IsOceania     ] = "Oceania"
      Value[IsSouthAmerica] = "South America"
      #---~---

      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( IsAfrica       | IsAntarctica   | IsAsia
                                 | IsEurope       | IsNorthAmerica | IsOceania
                                 | IsSouthAmerica
                                 )#end Valid

      Value[! IsValid] = NA_character_
      #---~---


      #---~---
      #   Standard names.
      #---~---
      OutAttribute = c("Africa","Antarctica","Asia","Europe","North America"
                      ,"Oceania","South America"
                      )#end c
      #---~---
   }else if (AncilDisturb){
      #---~---
      #   Site disturbance
      #---~---


      #---~---
      #   Make results case insensitive
      #---~---
      Value    = tolower(Value)
      #---~---


      #--- Discard invalid or ambiguous data.
      Discard        = ( (! grepl(pattern="[a-z]",x=Value) )
                       | ( Value %in% c( ) )
                       )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   Standardise and simplify disturbance categories.
      #---~---
      IsIntact  = Value %in% c( "undisturbed")
      IsGrazed  = Value %in% c( "cattle grazing", "levels of grazing and n-fertilizer"
                              , "low grazing", "moderate grazing", "high grazing")
      IsBurnt   = Value %in% c( "recently burned"
                              , "excluded from cattle grazing and burning since 1993"
                              , "frequent fire in grassland; not inside forest"
                              , "grassland areas burned regularly (3-5 years frequency)"
                              , "frequently burned")
      IsFlooded = Value %in% c( "frequently flooded")
      IsWind    = Value %in% c( "recent wind-throw")
      IsManaged = Value %in% c( "mechanical weed control", "recent precommercial thinning")
      IsCleared = Value %in% c( "recently clearcut")
      IsMined   = Value %in% c( "gypsum mine")
      IsOther   = Value %in% c( "other disturbance")
      #---~---



      #---~---
      #   Standardise disturbance names.
      #---~---
      Value[IsIntact ] = "01 - No Recent Disturbance"
      Value[IsGrazed ] = "02 - Grazed"
      Value[IsBurnt  ] = "03 - Burnt"
      Value[IsFlooded] = "04 - Flooded"
      Value[IsWind   ] = "05 - Wind-throw"
      Value[IsManaged] = "06 - Thinned"
      Value[IsCleared] = "07 - Clear-cut"
      Value[IsMined  ] = "08 - Mining"
      Value[IsOther  ] = "09 - Other"
      #---~---

      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( IsIntact  | IsGrazed  | IsBurnt   | IsFlooded | IsWind
                                 | IsManaged | IsCleared | IsMined   | IsOther   )
      Value[! IsValid] = NA_character_
      #---~---


      #---~---
      #   Standard names.
      #---~---
      OutAttribute = c("01 - No Recent Disturbance","02 - Grazed","03 - Burnt"
                      ,"04 - Flooded","05 - Wind-throw","06 - Thinned","07 - Clear-cut"
                      ,"08 - Mining","09 - Other"
                      )#end c
      #---~---


   }else if (AncilSunShade){
      #---~---
      #   Canopy position: sun vers. Shade leaf qualifier; light exposure
      #   Exposition light / irradiance
      #   Exposition: position of plant in the canopy
      #   Leaf exposition
      #---~---


      #---~---
      #   Logical flags to identify which ancillary variable we are loading.
      #---~---
      IsCanopyPos = DataID %in%  443L
      IsExpoLight = DataID %in% 2111L
      IsExpoPos   = DataID %in%  766L
      IsExpoLeaf  = DataID %in%  210L
      #---~---


      #---~---
      #   Make results case insensitive
      #---~---
      Value    = tolower(Value)
      #---~---


      #---~---
      #   Some authors provided information not directly translatable to leaf exposure.
      #---~---
      IsTMI = ( ( ( AuthorName %in% "Jens Kattge"    ) & ( NameOrig %in% "treeposition" ) )
              | ( ( AuthorName %in% "Jens Kattge"    ) & ( NameOrig %in% "leafposition" ) )
              | ( ( AuthorName %in% "Belinda Medlyn" ) & ( NameOrig %in% "understory"   ) )
              | ( ( AuthorName %in% "Han Chen"       ) & ( NameOrig %in% "mspos$"       ) )
              )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---


      #---~---
      #   Some authors used numeric code systems to assess canopy position or illumination.
      #---~---
      #   Benjamin Blonder: some entries used Boolean data
      #---~---
      IsAuthor = ( AuthorName %in% "Benjamin Blonder" ) & ( NameOrig %in% "sunleavesonly")
      Value[IsAuthor & (Value %in% "0")] = "partial shade"
      Value[IsAuthor & (Value %in% "1")] = "sun exposed"
      #---~---
      #   Chris Baraloto | S. Joseph Wright: canopy exposure index based on plant position
      # in the canopy.
      #---~---
      IsAuthor = ( ( ( AuthorName %in% "Chris Baraloto" )
                   & ( NameOrig %in% c("id_arbre","id_ram") ) 
                   )#end AuthorName
                 | ( ( AuthorName %in% "S. Joseph Wright" )
                   & ( NameOrig %in% "exposition: position of plant in the canopy")
                   )#end AuthorName
                 )#end IsAuthor
      Value[IsAuthor & (Value %in% "1"       )] = "full shade"
      Value[IsAuthor & (Value %in% c("2","3"))] = "partial shade"
      Value[IsAuthor & (Value %in% c("4","5"))] = "sun exposed"
      Discard = IsAuthor & (Value %in% "-9")
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---
      #   Johannes Cornelissen. Yes/no flag for sun/shade.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Johannes Cornelissen" )
                 & ( NameOrig %in% "shade.y.or.n")
                 )#end IsAuthor
      Value[IsAuthor & (Value %in% "y")] = "full shade"
      Value[IsAuthor & (Value %in% "n")] = "sun exposed"
      #---~---
      #   Anh Tuan Dang-Le: Light environment: 
      # h - high light (canopy openness 70%)
      # l - low light (canopy openness 30%)"
      #---~---
      IsAuthor = ( AuthorName %in% "Anh Tuan Dang-Le" ) & ( NameOrig %in% "le")
      Value[IsAuthor & (Value %in% "h")] = "sun exposed"
      Value[IsAuthor & (Value %in% "l")] = "full shade"
      #---~---
      #   Daniel Falster: code system for canopy position
      # 0 - supressed
      # 1 - intermediate
      # 2 - codominant (crown partly exposed)
      # 3 - dominant (crown fully exposed)
      # Data also has values above 3, discard information.
      #---~---
      IsAuthor = ( AuthorName %in% "Daniel Falster" ) & ( NameOrig %in% "status")
      Value[IsAuthor & (Value %in%       "0" )] = "full shade"
      Value[IsAuthor & (Value %in% c("1","2"))] = "partial shade"
      Value[IsAuthor & (Value %in%       "3" )] = "sun exposed"
      Discard  = IsAuthor & (Value %in% c("4","5","6","7","8","9"))
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---
      #   Daniel Falster / Isabelle Aubin: light levels
      #   For continuous levels, use 20 and 80% thresholds for exposure
      #---~---
      IsAuthor = 
         ( ( ( AuthorName %in% "Daniel Falster" ) & ( NameOrig %in% "light") )
         | ( ( AuthorName %in% "Isabelle Aubin" )
           & ( NameOrig %in% "exposition light / irradiance")
           )#end AuthorName
         )#end IsAuthor
      suppressWarnings({
         IsShade   = IsAuthor & ( as.numeric(Value) %lt% 20      )
         IsPartial = IsAuthor & ( as.numeric(Value) %wr% c(20,80))
         IsSun     = IsAuthor & ( as.numeric(Value) %gt% 80      )
      })#end suppressWarnings
      Value[IsShade  ] = "full shade"
      Value[IsPartial] = "partial shade"
      Value[IsSun    ] = "sun exposed"
      #---~---
      #   Aaron Kamoske: canopy position. Only first letter is provided.  The description
      # is truncated, using the most likely definition...
      #---~---
      IsAuthor = ( AuthorName %in% "Aaron Kamoske" ) & ( NameOrig %in% "canopy position")
      Value[IsAuthor & (Value %in% "b")] = "lower canopy"
      Value[IsAuthor & (Value %in% "m")] = "middle canopy"
      Value[IsAuthor & (Value %in% "t")] = "upper canopy"
      #---~---
      #   Jeremy Lichstein: canopy position. Most of the classes are generic, but we ought
      # to invalidate the data with "unknown".
      #---~---
      IsAuthor = ( AuthorName %in% "Jeremy Lichstein" ) & ( NameOrig %in% "lflight")
      Value[IsAuthor & (Value %in% "gap"  )] = "sun exposed"
      Value[IsAuthor & (Value %in% "shade")] = "full shade"
      Value[IsAuthor & (Value %in% "sun"  )] = "sun exposed"
      Discard  = IsAuthor & (Value %in% "unknown" )
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---
      #   Jon Lloyd: canopy position. Only first letter is provided, using best guess.
      #---~---
      IsAuthor = ( AuthorName %in% "Jon Lloyd" ) & ( NameOrig %in% "leafposition")
      Value[IsAuthor & (Value %in% "l")] = "lower canopy"
      Value[IsAuthor & (Value %in% "m")] = "middle canopy"
      Value[IsAuthor & (Value %in% "u")] = "upper canopy"
      #---~---
      #   Aristotelis C. Papageorgiou: they used light/shade flags. No partial shade
      # was provided, so we assume either full sun exposure or full shade.
      #---~---
      IsAuthor = 
         ( AuthorName %in% "Aristotelis C. Papageorgiou" ) & ( NameOrig %in% "shading")
      Value[IsAuthor & (Value %in% "shade")] = "full shade"
      Value[IsAuthor & (Value %in% "light")] = "sun exposed"
      #---~---
      #   Enio Sosinski: canopy index, but values are continuous rather than categorical.
      #   1 - under storey tree, fully shaded.
      #   2 - under storey tree, with some exposure
      #   3 - canopy tree, 10-90% of exposition. 
      #   4 - canopy tree, at least 90-99% of exposition.
      #   5 - emergent tree, entirely exposed to light
      #---~---
      IsAuthor = ( AuthorName %in% "Enio Sosinski" ) & ( NameOrig %in% "canopy")
      suppressWarnings({
         IsShade   = IsAuthor & ( as.numeric(Value) %lt% 1.9        )
         IsPartial = IsAuthor & ( as.numeric(Value) %wr% c(1.9,3.75))
         IsSun     = IsAuthor & ( as.numeric(Value) %gt% 3.75       )
      })#end suppressWarnings
      Value[IsShade  ] = "full shade"
      Value[IsPartial] = "partial shade"
      Value[IsSun    ] = "sun exposed"
      #---~---
      #   Elisa Van Cleemput: data were obtained for vegetation consisting of mostly
      # herbs and forbs, assume that "little shade" means fully exposed
      #---~---
      IsAuthor = ( AuthorName %in% "Elisa Van Cleemput" ) & ( NameOrig %in% "shading")
      Value[IsAuthor & ( Value %in% "little shade" )] = "sun exposed"
      #---~---


      #---~---
      #   Some authors provided names that could be ambiguous. 
      # Fix on an author-by-author basis.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Patrick Meir"                   )
                 & ( NameOrig   %in% c("treeposition","leafposition") )
                 )#end IsAuthor
      Value[IsAuthor & (Value %in% "ground")] = "understorey"
      #---~---


      #--- Discard invalid or ambiguous data.
      Discard        = 
        ( (! grepl(pattern="[a-z]",x=Value) )
        | ( Value %in% c( "1.3 m", "1.5 m high. south facing", "not identified") )
        )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---



      #---~---
      #   Standardise names
      #---~---
      IsShade   = 
         Value %in% c( "3-5% of par", "5% of par", "8-10% of par", "10% of par"
                     , "18-20% of par", "bottom", "bottom canopy; shaded", "full shade"
                     , "low", "lower", "lower canopy", "lumire biais_e", "shade"
                     , "shade leaves", "shaded", "under 8% ppfd", "under 20% ppfd"
                     , "understory", "understorey", "understorey shaded"
                     )#end c
      IsPartial =
         Value %in% c( "34% of par", "55-60% of par", "basal-interior canopy leaves"
                     , "between 20 and 80% ppfd", "mid", "middle"
                     , "middle canopy", "partial shade"
                     , "partial shade/full shade", "sun&shade", "sun and shade"
                     )#end c
      IsSun     =
         Value %in% c( "100% of par", "basal-exterior canopy leaves", "canopy", "edge"
                     , "full", "full light (bare ground)", "not shaded/partial shade"
                     , "full overhead light along logging road or in tree fall gap"
                     , "fullsun", "full-sun", "fully sunlit", "gap", "not shaded"
                     , "mid-upper", "open", "open condition (road side)", "overstory", "sun"
                     , "sun exposed", "sun leaves", "sun-lit", "sun only", "top"
                     , "top - flag leaf", "top canopy", "top canopy leaves"
                     , "top canopy; sun exposed", "top sunlit", "top_leaf u01"
                     , "top_leaf u02", "top_leaf u03", "top_leaf u04", "upper"
                     , "upper canopy", "uppermost branch"
                     )#end c
      #---~---



      #---~---
      #   Standardise sun/shade names.
      #---~---
      Value[IsShade  ] = "01 - Mostly Shaded"
      Value[IsPartial] = "02 - Partially Shaded"
      Value[IsSun    ] = "03 - Mostly Sun-Exposed"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = Valid & ( IsShade   | IsPartial | IsSun     )
      Value[! IsValid] = NA_character_
      #---~---



      #---~---
      #   Standard names.
      #---~---
      OutAttribute = c("01 - Mostly Shaded"
                      ,"02 - Partially Shaded"
                      ,"03 - Mostly Sun-Exposed"
                      )#end c
      #---~---
   }else if (AncilBiome){
      #---~---
      #   Vegetation type / Biome
      #   Vegetation type / Biome ( 2)
      #---~---



      #---~---
      #   Make results case insensitive
      #---~---
      Value    = tolower(Value)
      #---~---


      #---~---
      #   Some authors provide additional information or information not exactly linked to
      # biomes, or too generic description that could be misleading, skip them.
      #---~---
      IsTMI = 
         ( ( ( AuthorName %in% "Anh Tuan Dang-Le"    ) & ( NameOrig %in% "vegtype"   ) )
         | ( ( AuthorName %in% "Robert Jackson"      ) & ( NameOrig %in% "k_ppen_cc" ) )
         | ( ( AuthorName %in% "Yusuke Onoda"        ) & ( NameOrig %in% "biome"     ) )
         | ( ( AuthorName %in% "Peter Reich"         ) & ( NameOrig %in% "newbiomes" ) )
         | ( ( AuthorName %in% "Andrew Cunliffe"     )
             & ( NameOrig %in% "igbp_class_long"     )
             )#end AuthorName
         | ( ( AuthorName %in% "Fritz Schweingruber" )
             & ( NameOrig %in% "vegetation zone"     )
             )#end AuthorName
         | ( ( AuthorName %in% "Serge Sheremetev"    )
           & ( NameOrig   %in% c( "community", "biome") )
           )#end AuthorName
         | ( ( AuthorName %in% c("Yan-Shih Lin","Belinda Medlyn" ) )
           & ( NameOrig   %in% c("tregion","wregion") )
           )#end AuthorName
         )#end IsTMI
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #---~---


      #---~---
      #   Some authors provided climate instead of biome. Assign climate to a trait
      # variable and send data to the write trait column.
      #---~---
      #   Colleen Iversen.  They provided the complete climate classes.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Colleen Iversen" ) 
                 & ( NameOrig %in% "climate_koeppen-geiger classification" )
                 )#end IsAuthor
      TraitID[IsAuthor] = 825L
      Trait  [IsAuthor & (Value %in% "af" )] = "Af"
      Trait  [IsAuthor & (Value %in% "am" )] = "Am"
      Trait  [IsAuthor & (Value %in% "as" )] = "As"
      Trait  [IsAuthor & (Value %in% "aw" )] = "Aw"
      Trait  [IsAuthor & (Value %in% "bsh")] = "BSh"
      Trait  [IsAuthor & (Value %in% "bsk")] = "BSk"
      Trait  [IsAuthor & (Value %in% "bwh")] = "BWh"
      Trait  [IsAuthor & (Value %in% "bwk")] = "BWk"
      Trait  [IsAuthor & (Value %in% "cfa")] = "Cfa"
      Trait  [IsAuthor & (Value %in% "cfb")] = "Cfb"
      Trait  [IsAuthor & (Value %in% "cfc")] = "Cfc"
      Trait  [IsAuthor & (Value %in% "csa")] = "Csa"
      Trait  [IsAuthor & (Value %in% "csb")] = "Csb"
      Trait  [IsAuthor & (Value %in% "cwa")] = "Cwa"
      Trait  [IsAuthor & (Value %in% "cwb")] = "Cwb"
      Trait  [IsAuthor & (Value %in% "dfa")] = "Dfa"
      Trait  [IsAuthor & (Value %in% "dfb")] = "Dfb"
      Trait  [IsAuthor & (Value %in% "dfc")] = "Dfc"
      Trait  [IsAuthor & (Value %in% "dfd")] = "Dfd"
      Trait  [IsAuthor & (Value %in% "dsb")] = "Dsb"
      Trait  [IsAuthor & (Value %in% "dsc")] = "Dsc"
      Trait  [IsAuthor & (Value %in% "dwa")] = "Dwa"
      Trait  [IsAuthor & (Value %in% "dwb")] = "Dwb"
      Trait  [IsAuthor & (Value %in% "dwc")] = "Dwc"
      Trait  [IsAuthor & (Value %in% "et" )] = "ET"
      Value  [IsAuthor] = NA_character_
      Valid  [IsAuthor] = FALSE
      VName  [IsAuthor] = NA_character_
      #---~---
      #   Pengcheng He.  They provided the complete climate classes.
      #---~---
      IsAuthor          = ( AuthorName %in% "Pengcheng He" ) & ( NameOrig %in% "climate" )
      TraitID[IsAuthor] = 825L
      Trait  [IsAuthor & (Value %in% "southern subtropical monsoon climate")] = "Cfa"
      Value  [IsAuthor] = NA_character_
      Valid  [IsAuthor] = FALSE
      VName  [IsAuthor] = NA_character_
      #---~---


      #---~---
      #   Some authors provided too generic names but data comes from specific regions.
      # Rename classes to remove ambiguity.
      #---~---
      #   Owen Atkin: replace short names with unambiguous classes.
      #---~---
      IsAuthor = ( AuthorName %in% "Owen Atkin" ) & ( NameOrig %in% "biome" )
      Value[IsAuthor & ( Value %in% "bf"     )] = "boreal forest"
      Value[IsAuthor & ( Value %in% "sa"     )] = "tropical savannah"
      Value[IsAuthor & ( Value %in% "tedf"   )] = "temperate deciduous forest"
      Value[IsAuthor & ( Value %in% "teg"    )] = "temperate grassland"
      Value[IsAuthor & ( Value %in% "terf"   )] = "temperate rainforest"
      Value[IsAuthor & ( Value %in% "tew"    )] = "temperate woodland"
      Value[IsAuthor & ( Value %in% "trrf_lw")] = "lowland tropical rainforest"
      Value[IsAuthor & ( Value %in% "trrf_up")] = "montane tropical rainforest"
      Value[IsAuthor & ( Value %in% "tu"     )] = "tundra"
      #---~---
      #   William Bond.  Contributed data are for South Africa.
      #---~---
      IsAuthor = ( AuthorName %in% "William Bond" ) & ( NameOrig %in% "habitat" )
      Value[IsAuthor & ( Value %in% "forest")] = "subtropical forest"
      #---~---
      #   Hans Henrik Bruun.  Contributed data are for sub-arctic
      #---~---
      IsAuthor = ( AuthorName %in% "Hans Henrik Bruun" ) & ( NameOrig %in% "vegetation type" )
      Value[IsAuthor & ( Value %in% "boreal river bank"                    )] = "boreal forest"
      Value[IsAuthor & ( Value %in% "low alpine dry rich heath"            )] = "boreal grassland"
      Value[IsAuthor & ( Value %in% "low alpine mesic heath"               )] = "boreal grassland"
      Value[IsAuthor & ( Value %in% "low alpine poor heath"                )] = "boreal grassland"
      Value[IsAuthor & ( Value %in% "middle alpine grass heath"            )] = "boreal grassland"
      Value[IsAuthor & ( Value %in% "subalpine birch forest of heath type" )] = "boreal forest"
      Value[IsAuthor & ( Value %in% "subalpine birch forest of meadow type")] = "boreal forest"
      #---~---
      #   Johannes Cornelissen.  Contributed data are for sub-arctic
      #---~---
      IsAuthor = ( AuthorName %in% "Johannes Cornelissen" ) & ( NameOrig %in% "habitat" )
      Value[IsAuthor & ( Value %in% "forest"   )] = "boreal forest"
      Value[IsAuthor & ( Value %in% "grassland")] = "boreal grassland"
      Value[IsAuthor & ( Value %in% "woodland" )] = "boreal woodland"
      #---~---
      #   Joseph Hawes.  Generic names in Portuguese, but all data came from the Amazon.
      #                  Assume tropical.
      #---~---
      IsAuthor = ( AuthorName %in% "Joseph Hawes" ) & ( NameOrig %in% "vegetation_type" )
      IsTropMoFor  = 
         ( IsAuthor
         & ( Value %in% c( "capoeira", "capoeira velha", "floresta de terra firme"
                         , "floresta primaria", "floresta primaria alterada"
                         , "floresta primaria de terra firme", "floresta secundaria"
                         , "floresta secundaria de terra firme", "vegetacao remanescente"
                         )#end c
           )#end Value
         )#end IsTropMoFor
      IsWetlands   = 
         IsAuthor & ( Value %in% c( "capoeira grossa em igapo", "igapo", "varzea") )
      IsPlantation = IsAuthor & ( Value %in% c("vegetacao cultivada") )
      Value[IsTropMoFor ] = "tropical moist forest"
      Value[IsWetlands  ] = "swamp forest"
      Value[IsPlantation] = "plantation"
      #---~---
      #   Tianhua He.  Contributed data are for western Australia. Assume Mediterranean.
      #---~---
      IsAuthor        = (AuthorName %in% "Tianhua He") & (NameOrig %in% "vegetation type")
      Value[IsAuthor] = "mediterranean"
      #---~---
      #   Thomas Hickler.  Contributed data are for southern Sweden. Assume temperate.
      #---~---
      IsAuthor           = ( ( AuthorName %in% "Thomas Hickler"  )
                           & ( NameOrig   %in% "vegetation type" )
                           )#end IsAuthor
      IsTempScrub        = IsAuthor & ( Value %in% "grassland / shrubland" )
      Value[IsTempScrub] = "temperate scrubland"
      #---~---
      #   Robert Jackson. Rename ambiguous entries based on inspecting the data base.
      #---~---
      IsAuthor = ( AuthorName %in% "Robert Jackson" ) & ( NameOrig   %in% c("vegetation") )
      Value[IsAuthor & ( Value %in% "broad-leaved evergreen forest" )] = "sub-tropical moist forest"
      Value[IsAuthor & ( Value %in% "coniferous evergreen forest"   )] = "sub-tropical moist forest"
      Value[IsAuthor & ( Value %in% "deciduous broad-leaved forests")] = "sub-tropical moist forest"
      Value[IsAuthor & ( Value %in% "deciduous forest"              )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "dry deciduous forest"          )] = "sub-tropical dry forest"
      Value[IsAuthor & ( Value %in% "forest"                        )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "montane rainforest"            )] = "tropical moist forest"
      Value[IsAuthor & ( Value %in% "natural oak forest"            )] = "sub-tropical moist forest"
      Value[IsAuthor & ( Value %in% "rain forest"                   )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "semi-decid rain forest"        )] = "tropical moist forest"
      Value[IsAuthor & ( Value %in% "shrubland"                     )] = "temperate scrubland"
      #---~---
      #   Steven Jansen.  Some classes have short names. Make longer names to avoid ambiguity.
      #---~---
      IsAuthor = ( AuthorName %in% "Steven Jansen" ) & ( NameOrig   %in% "biome" )
      Value[IsAuthor & (Value %in% "trr")] = "tropical moist forest"
      Value[IsAuthor & (Value %in% "trs")] = "tropical dry forest"
      Value[IsAuthor & (Value %in% "tmr")] = "temperate rain forest"
      Value[IsAuthor & (Value %in% "tms")] = "temperate deciduous forest"
      Value[IsAuthor & (Value %in% "wds")] = "temperate woodland"
      Value[IsAuthor & (Value %in% "des")] = "desert"
      Value[IsAuthor & (Value %in% "bot")] = "boreal-tundra"
      #---~---
      #   Dushan Kumarathunge. Most data came from forests, but there were Mediterranean and
      # non-forest ecosystems. Keep tundra but discard the others.
      #---~---
      IsAuthor = ( AuthorName %in% "Dushan Kumarathunge" ) & ( NameOrig   %in% "tregion" )
      Discard  = IsAuthor & Value %in% c("boreal","temperate_n","temperate_s","tropical")
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---
      #   Vojtech Lanta.  Meadows, assume temperate.
      #---~---
      IsAuthor = ( AuthorName %in% "Vojtech Lanta" ) & ( NameOrig %in% "description" )
      Value[IsAuthor & ( Value %in% "dry meadow" )] = "temperate meadow"
      Value[IsAuthor & ( Value %in% "wet meadow" )] = "wet meadow"
      #---~---
      #   Maowei Liang.  Temperate, semi-arid range lands. Assume temperate grasslands.
      #---~---
      IsAuthor = ( AuthorName %in% "Maowei Liang" ) & ( NameOrig %in% "biome_types" )
      Value[IsAuthor & ( Value %in% "grasslands" )] = "temperate grassland"
      #---~---
      #   Adam Martin.  The data repository mentions forests. Assume forests.
      #---~---
      IsAuthor = ( AuthorName %in% "Adam Martin" ) & ( NameOrig %in% c("biome","biome.new") )
      Value[IsAuthor & ( Value %in% "subtropical_mediterranean" )] = "mediterranean"
      Value[IsAuthor & ( Value %in% "temperate_boreal"          )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "tropical"                  )] = "tropical forest"
      Value[IsAuthor & ( Value %in% "temperate"                 )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "boreal"                    )] = "boreal forest"
      #---~---
      #   Angela Moles. Rename ambiguous entries based on inspecting the data base.
      #---~---
      IsAuthor = ( AuthorName %in% "Angela Moles" ) & ( NameOrig   %in% c("vegetation") )
      Value[IsAuthor & ( Value %in% "sub-tropical and temperate rainforest")] = "temperate forest"
      Value[IsAuthor & ( Value %in% "tropical and subtropical grasslands"  )] = "sub-tropical grassland"
      #---~---
      #   Ulo Niinemets.  The data repository mentions forests. Assume Mediterranean.
      #---~---
      IsAuthor = ( AuthorName %in% "Ulo Niinemets" ) & ( NameOrig %in% "community type" )
      Value[IsAuthor & ( Value %in% "forest"                     )] = "mediterranean"
      Value[IsAuthor & ( Value %in% "forest/dry forest"          )] = "sub-tropical dry forest"
      Value[IsAuthor & ( Value %in% "dry forest"                 )] = "temperate woodland"
      Value[IsAuthor & ( Value %in% "dry forest?"                )] = "tropical dry forest"
      Value[IsAuthor & ( Value %in% "heathland"                  )] = "sub-tropical scrubland"
      Value[IsAuthor & ( Value %in% "lowland-shrubland"          )] = "temperate scrubland"
      Value[IsAuthor & ( Value %in% "mesic"                      )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "mesic; near stream"         )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "montane forest"             )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "montane to subalpine forest")] = "temperate forest"
      Value[IsAuthor & ( Value %in% "ridge"                      )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "savanna"                    )] = "sub-tropical savannah"
      Value[IsAuthor & ( Value %in% "savanna/dry forest?"        )] = "tropical dry forest"
      Value[IsAuthor & ( Value %in% "sclerophyll forest"         )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "sclerophyllous forest"      )] = "mediterranean"
      Value[IsAuthor & ( Value %in% "shrubland-forest"           )] = "temperate woodland"
      Value[IsAuthor & ( Value %in% "valley"                     )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "valley floor forest"        )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "woodland"                   )] = "temperate woodland"
      #---~---
      #   Juli Pausas.  They provide multiple classes, sometimes with multiple biomes in
      # the same entry. We only keep the unequivocal ones.
      #---~---
      IsAuthor = ( AuthorName %in% "Juli Pausas" ) & ( NameOrig %in% "biome" )
      IsTMI    = IsAuthor & 
         ( Value %in% c( "desert; med", "desert; med; tempbroad", "desert; med; trdry"
                       , "desert; tempbroad; med"
                       , "desert; tempconif; med", "desert; tempgrass"
                       , "desert; trconif", "desert; trconif; trdry", "desert; trgrass"
                       , "flgrass; tempconif", "flgrass; trgrass", "med; desert"
                       , "med; desert; tempbroad", "med; desert; tempconif; tempgrass"
                       , "med; desert; tempgrass"
                       , "med; desert; tempgrass; tempconif; trconif", "med; montgrass"
                       , "med; montgrass; trgrass", "med; tempbroad"
                       , "med; tempbroad; desert", "med; tempbroad; trmoist"
                       , "med; tempconif", "med; tempgrass", "med; trconif", "med; trgrass"
                       , "med; trmoist", "montgrass", "montgrass; desert"
                       , "montgrass; med; desert", "montgrass; tempbroad"
                       , "montgrass; trgrass", "montgrass; trgrass; trmoist"
                       , "montgrass; trgrass; trmoist; desert", "montgrass; trmoist"
                       , "montgrass; trmoist; trgrass", "montgrass;trgrass; trmoist"
                       , "several", "taiga; tempbroad", "taiga; tempconif"
                       , "taiga; tempgrass; tempconif", "taiga; tundra; trconif; med"
                       , "tempbroad; desert", "tempbroad; desert; med"
                       , "tempbroad; desert; montgrass", "tempbroad; med"
                       , "tempbroad; med; desert", "tempbroad; med; tempgrass; montgrass"
                       , "tempbroad; montgrass", "tempbroad; taiga"
                       , "tempbroad; taiga; tempconif", "tempbroad; taiga; tempgrass"
                       , "tempbroad; taiga; tundra", "tempbroad; tempconif; med"
                       , "tempbroad; tempconif; tempgrass", "tempbroad; tempconif; trmoist"
                       , "tempbroad; tempconif; trmoist; trconif", "tempbroad; tempgrass"
                       , "tempbroad; tempgrass; med", "tempbroad; tempgrass; montgrass"
                       , "tempbroad; tempgrass; taiga", "tempbroad; tempgrass; tempconif"
                       , "tempbroad; tempgrass; trgrass", "tempbroad; trgrass"
                       , "tempbroad; trmoist", "tempbroad; trmoist; trgrass"
                       , "tempbroad; tundra; taiga", "tempconif; desert"
                       , "tempconif; desert; tempgrass", "tempconif; med"
                       , "tempconif; med; tempbroad", "tempconif; taiga"
                       , "tempconif; taiga; med; desert", "tempconif; taiga; tempbroad"
                       , "tempconif; taiga; tempgrass", "tempconif; tempbroad; med"
                       , "tempconif; tempbroad; tempgrass", "tempconif; tempgrass; desert"
                       , "tempconif; tempgrass; med; desert"
                       , "tempconif; tempgrass; trconif", "tempconif; trconif"
                       , "tempgrass; desert", "tempgrass; desert; tempconif"
                       , "tempgrass; montgrass; tempbroad", "tempgrass; tempbroad"
                       , "tempgrass; tempbroad; desert", "tempgrass; tempbroad; taiga"
                       , "tempgrass; tempbroad; tempconif"
                       , "tempgrass; tempbroad; trconif", "tempgrass; tempconif"
                       , "tempgrass; tempconif; desert", "tempgrass; trgrass"
                       , "tempgrass; trgrass; tempbroad", "trconif; desert"
                       , "trconif; med", "trconif; med; desert"
                       , "trconif; trmoist; desert; trdry", "trdry; flgrass"
                       , "trdry; trconif; tempconif; trmoist", "trdry; trgrass"
                       , "trdry; trmoist", "trdry; trmoist; desert"
                       , "trdry; trmoist; trgrass", "trgrass; desert", "trgrass; montgrass"
                       , "trgrass; montgrass; trmoist", "trgrass; tempbroad; trmoist"
                       , "trgrass; tempgrass", "trgrass; tempgrass; med"
                       , "trgrass; tempgrass; tempbroad", "trgrass; trdry"
                       , "trgrass; trmoist", "trgrass; trmoist; montgrass"
                       , "trmoist; desert", "trmoist; montgrass"
                       , "trmoist; montgrass; trgrass"
                       , "trmoist; montgrass; trgrass; tempgrass", "trmoist; tempconif"
                       , "trmoist; trconif; tempconif", "trmoist; trdry"
                       , "trmoist; trdry; trconif", "trmoist; trdry; trgrass"
                       , "trmoist; trgrass", "trmoist; trgrass; trdry"
                       )#end c
         )#end Value
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Retain unambiguous classes (list all for completedness)
      Value[IsAuthor & ( Value %in% "desert"               )] = "desert"
      Value[IsAuthor & ( Value %in% "flgrass"              )] = "floodplain"
      Value[IsAuthor & ( Value %in% "mangrove"             )] = "mangrove"
      Value[IsAuthor & ( Value %in% "med"                  )] = "mediterranean"
      Value[IsAuthor & ( Value %in% "taiga; tundra"        )] = "tundra"
      Value[IsAuthor & ( Value %in% "tempbroad"            )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "tempbroad; tempconif" )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "tempconif"            )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "tempconif; tempbroad" )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "tempgrass"            )] = "temperate grassland"
      Value[IsAuthor & ( Value %in% "trbroad; trconif"     )] = "sub-tropical moist forest"
      Value[IsAuthor & ( Value %in% "trconif"              )] = "sub-tropical moist forest"
      Value[IsAuthor & ( Value %in% "trdry"                )] = "tropical dry forest"
      Value[IsAuthor & ( Value %in% "trgrass"              )] = "tropical grassland"
      Value[IsAuthor & ( Value %in% "trmoist"              )] = "tropical moist forest"
      Value[IsAuthor & ( Value %in% "trmoist; trconif"     )] = "sub-tropical moist forest"
      Value[IsAuthor & ( Value %in% "tundra"               )] = "tundra"
      #---~---
      #   Valerio Pillar.  The data repository is from subtropical South America.
      #---~---
      IsAuthor = ( AuthorName %in% "Valerio Pillar" ) & ( NameOrig %in% "ecosystem" )
      Value[IsAuthor & ( Value %in% "grassland" )] = "sub-tropical grassland"
      #---~---
      #   Hendrik Poorter.  Replace short names with unambiguous ones.
      #---~---
      IsAuthor = ( AuthorName %in% "Hendrik Poorter" ) & ( NameOrig %in% "biomeestim" )
      Value[IsAuthor & ( Value %in% "alp"    )] = "alpine"
      Value[IsAuthor & ( Value %in% "aqf"    )] = "aquatic"
      Value[IsAuthor & ( Value %in% "aqs"    )] = "aquatic"
      Value[IsAuthor & ( Value %in% "des"    )] = "desert"
      Value[IsAuthor & ( Value %in% "grl"    )] = "temperate grassland"
      Value[IsAuthor & ( Value %in% "grlalp" )] = "temperate grassland"
      Value[IsAuthor & ( Value %in% "mang"   )] = "mangrove"
      Value[IsAuthor & ( Value %in% "salt"   )] = "salt water"
      Value[IsAuthor & ( Value %in% "sav"    )] = "tropical savannah"
      Value[IsAuthor & ( Value %in% "shl"    )] = "temperate scrubland"
      Value[IsAuthor & ( Value %in% "shr"    )] = "tropical scrubland"
      Value[IsAuthor & ( Value %in% "tedf"   )] = "temperate deciduous forest"
      Value[IsAuthor & ( Value %in% "terf"   )] = "temperate rain forest"
      Value[IsAuthor & ( Value %in% "trdf"   )] = "tropical dry forest"
      Value[IsAuthor & ( Value %in% "trrf"   )] = "tropical rain forest"
      Value[IsAuthor & ( Value %in% "trrh"   )] = "tropical rain forest"
      Value[IsAuthor & ( Value %in% "tun"    )] = "tundra"
      Value[IsAuthor & ( Value %in% "wol"    )] = "tropical savannah"
      #---~---
      #   Peter Reich. Rename short entries based on reference paper.
      #---~---
      IsAuthor = ( AuthorName %in% "Peter Reich" ) & ( NameOrig   %in% c("biome") )
      Value[IsAuthor & ( Value %in% "b"    )] = "boreal forest"
      Value[IsAuthor & ( Value %in% "medi" )] = "mediterranean"
      Value[IsAuthor & ( Value %in% "te"   )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "tr"   )] = "tropical forest"
      Value[IsAuthor & ( Value %in% "trsav")] = "tropical savannah"
      #---~---
      #   Fritz Schweingruber.  Replace short names with unambiguous ones.
      #---~---
      IsAuthor = ( AuthorName %in% "Fritz Schweingruber" ) & ( NameOrig %in% "veg type" )
      Value[IsAuthor & ( Value %in% "a arctic"            )] = "tundra"
      Value[IsAuthor & ( Value %in% "alpine"              )] = "alpine"
      Value[IsAuthor & ( Value %in% "b alpine"            )] = "alpine"
      Value[IsAuthor & ( Value %in% "c boreal"            )] = "boreal forest"
      Value[IsAuthor & ( Value %in% "d mountain"          )] = "alpine"
      Value[IsAuthor & ( Value %in% "e hill and mountain" )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "f hill"              )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "g mediterranean"     )] = "mediterranean"
      Value[IsAuthor & ( Value %in% "h"                   )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "h arid"              )] = "desert"
      Value[IsAuthor & ( Value %in% "hill"                )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "i subtropical"       )] = "sub-tropical forest"
      Value[IsAuthor & ( Value %in% "j tropical"          )] = "tropical forest"
      Value[IsAuthor & ( Value %in% "mediterranean"       )] = "mediterranean"
      Value[IsAuthor & ( Value %in% "nile"                )] = "desert"
      Value[IsAuthor & ( Value %in% "tropical"            )] = "tropical forest"
      #---~---
      #   Colleen Seymour.  Data provided are for Miombo ecosystems.
      #   Assume all tropical savannah.
      #---~---
      IsAuthor = ( AuthorName %in% "Colleen Seymour" ) & ( NameOrig %in% "habitat-cpg" )
      Value[IsAuthor] = "tropical savannah"
      #---~---
      #   Bill Shipley.  Data provided are from Quebec.
      #   Assume non-bog to be temperate forest.
      #---~---
      IsAuthor = ( AuthorName %in% "Bill Shipley" ) & ( NameOrig %in% "bog" )
      Value[IsAuthor & ( Value %in% "no bog")] = "temperate forest"
      Value[IsAuthor & ( Value %in% "yes"   )] = "bog"
      #---~---
      #   Andrew Siefert.  Data provided are from New York.
      #   Assume everything to be temperate forest.
      #---~---
      IsAuthor = ( AuthorName %in% "Andrew Siefert" ) & ( NameOrig %in% "vegetation type" )
      Value[IsAuthor] = "temperate forest"
      #---~---
      #   Nadejda Soudzilovskaia.  Data provided are from the Caucasus.
      #   Assume everything to be temperate/boreal ecosystems (rename only ambiguous entries).
      #---~---
      IsAuthor = ( ( AuthorName %in% "Nadejda Soudzilovskaia"         )
                 & ( NameOrig   %in% c("community","vegetation type") )
                 )#end IsAuthor
      Value[IsAuthor & ( Value %in% "dry grassland"        )] = "temperate grassland"
      Value[IsAuthor & ( Value %in% "forests"              )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "meadow"               )] = "temperate meadow"
      Value[IsAuthor & ( Value %in% "meadow in forest gap" )] = "temperate woodland"
      Value[IsAuthor & ( Value %in% "mountain forest"      )] = "temperate forest"
      Value[IsAuthor & ( Value %in% "mountain meadow"      )] = "temperate meadow"
      Value[IsAuthor & ( Value %in% "mountain steppe"      )] = "temperate steppe"
      Value[IsAuthor & ( Value %in% "open woodland"        )] = "temperate woodland"
      Value[IsAuthor & ( Value %in% "short-grass meadow"   )] = "temperate meadow"
      Value[IsAuthor & ( Value %in% "shrubland"            )] = "temperate scrubland"
      Value[IsAuthor & ( Value %in% "tall herb meadow"     )] = "temperate meadow"
      #---~---
      #   Evert Thomas. They provided "bioregions", but the study is focussed on 
      # tropical dry forests in Colombia.
      #---~---
      IsAuthor = ( AuthorName %in% "Evert Thomas" ) & ( NameOrig   %in% c("bioregion") )
      Value[IsAuthor & ( Value %in% "valle del cauca"    )] = "tropical dry forest"
      Value[IsAuthor & ( Value %in% "caribe"             )] = "tropical dry forest"
      Value[IsAuthor & ( Value %in% "chicamocha"         )] = "tropical dry forest"
      Value[IsAuthor & ( Value %in% "tatacoa"            )] = "tropical dry forest"
      Value[IsAuthor & ( Value %in% "valle del magdalena")] = "tropical dry forest"
      Value[IsAuthor & ( Value %in% "valle del patia"    )] = "tropical dry forest"
      #---~---
      #   David Tng. Trait data from this study were collected for testing whether 
      # Eucalyptus forests in Australia were evolutionarily closer to savannahs or 
      # forests, hence the specific classes. The study found Eucalyptus forest to be
      # more akin to moist forests, hence the classification adopted.
      #---~---
      IsAuthor = ( AuthorName %in% "David Tng" ) & ( NameOrig   %in% c("vegetation type") )
      Value[IsAuthor & ( Value %in% "tropical eucalipt forest" )] = "tropical moist forest"
      Value[IsAuthor & ( Value %in% "tropical eucalypt forest" )] = "tropical moist forest"
      Value[IsAuthor & ( Value %in% "temperate eucalypt forest")] = "temperate forest"
      Value[IsAuthor & ( Value %in% "temperate savanna"        )] = "temperate woodland"
      #---~---
      #   Oscar Valverde-Barrantes. Meta data base, we try to assign biomes to the best
      # of our knowledge (using the original class names, species, and/or the original 
      # references).
      #---~---
      IsAuthor = AuthorName %in% "Oscar Valverde-Barrantes"
      IsEcos   = IsAuthor & ( NameOrig   %in% "ecosystem" )
      IsBiome  = IsAuthor & ( NameOrig   %in% "biome"     )
      #--- Root data (original name is "Ecosystem")
      Value[IsBiome & ( Value %in%  "boreal"                   )] = "boreal scrubland"
      Value[IsBiome & ( Value %in%  "grassland"                )] = "temperate grassland"
      Value[IsBiome & ( Value %in%  "mediterranean"            )] = "mediterranean"
      Value[IsBiome & ( Value %in%  "subalpine"                )] = "temperate grassland"
      Value[IsBiome & ( Value %in%  "subtropical forest"       )] = "sub-tropical forest"
      Value[IsBiome & ( Value %in%  "temperate forest"         )] = "temperate forest"
      Value[IsBiome & ( Value %in%  "tropical forest"          )] = "tropical forest"
      #--- Leaf vein data (original name is "Biome")
      Value[IsEcos  & ( Value %in% "aerial_leaves"             )] = "aquatic"
      Value[IsEcos  & ( Value %in% "alpine__shrub"             )] = "boreal scrubland"
      Value[IsEcos  & ( Value %in% "alpine_herb"               )] = "temperate grassland"
      Value[IsEcos  & ( Value %in% "alpine_herb_"              )] = "temperate grassland"
      Value[IsEcos  & ( Value %in% "aquatic_c_herb"            )] = "aquatic"
      Value[IsEcos  & ( Value %in% "canopy_y_tree"             )] = "sub-tropical forest"
      Value[IsEcos  & ( Value %in% "cloud_forest"              )] = "tropical forest"
      Value[IsEcos  & ( Value %in% "cloud_orests"              )] = "tropical forest"
      Value[IsEcos  & ( Value %in% "coastal_forest"            )] = "temperate forest"
      Value[IsEcos  & ( Value %in% "deciduous_forest"          )] = "temperate forest"
      Value[IsEcos  & ( Value %in% "desert__shrub"             )] = "desert"
      Value[IsEcos  & ( Value %in% "dry_forest"                )] = "tropical dry forest"
      Value[IsEcos  & ( Value %in% "evergreen__shrub"          )] = "sub-tropical scrubland"
      Value[IsEcos  & ( Value %in% "evergreen_forest"          )] = "temperate forest"
      Value[IsEcos  & ( Value %in% "floating_leaves"           )] = "aquatic"
      Value[IsEcos  & ( Value %in% "grasslands"                )] = "temperate grassland"
      Value[IsEcos  & ( Value %in% "lowland_forest"            )] = "tropical forest"
      Value[IsEcos  & ( Value %in% "mediterranean"             )] = "mediterranean"
      Value[IsEcos  & ( Value %in% "mesic_forest"              )] = "temperate forest"
      Value[IsEcos  & ( Value %in% "montane_forest"            )] = "sub-tropical forest"
      Value[IsEcos  & ( Value %in% "premontane_forest"         )] = "sub-tropical forest"
      Value[IsEcos  & ( Value %in% "premontanetropical_forest" )] = "sub-tropical forest"
      Value[IsEcos  & ( Value %in% "rainforest_t_tree"         )] = "tropical moist forest"
      Value[IsEcos  & ( Value %in% "sclerophyll_forest"        )] = "tropical forest"
      Value[IsEcos  & ( Value %in% "semi-desert"               )] = "sub-tropical scrubland"
      Value[IsEcos  & ( Value %in% "subtropical_forest"        )] = "subtropical forest"
      Value[IsEcos  & ( Value %in% "subtropical_orests"        )] = "sub-tropical forest"
      Value[IsEcos  & ( Value %in% "succulent"                 )] = "desert"
      Value[IsEcos  & ( Value %in% "temperate"                 )] = "temperate forest"
      Value[IsEcos  & ( Value %in% "temperate_e_zone"          )] = "temperate forest"
      Value[IsEcos  & ( Value %in% "temperate_forest"          )] = "temperate forest"
      Value[IsEcos  & ( Value %in% "temperate_orests"          )] = "temperate forest"
      Value[IsEcos  & ( Value %in% "tropical_desert"           )] = "desert"
      Value[IsEcos  & ( Value %in% "tropical_dry"              )] = "tropical dry forest"
      Value[IsEcos  & ( Value %in% "tropical_forest"           )] = "tropical forest"
      Value[IsEcos  & ( Value %in% "tropical_montane"          )] = "tropical forest"
      Value[IsEcos  & ( Value %in% "tropical_orests"           )] = "tropical moist forest"
      Value[IsEcos  & ( Value %in% "tropical_wet"              )] = "tropical moist forest"
      Value[IsEcos  & ( Value %in% "understory_treams"         )] = "temperate forest"
      Value[IsEcos  & ( Value %in% "wet_forest"                )] = "sub-tropical forest"
      Value[IsEcos  & ( Value %in% "wet_orests"                )] = "sub-tropical forest"
      Value[IsEcos  & ( Value %in% "wetland"                   )] = "wetland"
      Value[IsEcos  & ( Value %in% "wetlands"                  )] = "wetland"
      #---~---
      #   Stephni van der Merwe. Their measurements took place in Marion Island and 
      # mainland South Africa
      #---~---
      IsAuthor = ( AuthorName %in% "Stephni van der Merwe" ) & ( NameOrig   %in% "biome" )
      Value[IsAuthor & ( Value %in% "grassland"            )] = "sub-tropical grassland"
      Value[IsAuthor & ( Value %in% "sub-antarctic tundra" )] = "tundra"

      #---~---
      #   Ian Wright.  Data provided are mostly from Australia and New Zealand.
      #   Rename ambiguous entries based on inspecting the data base.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Ian Wright" )
                 & ( NameOrig   %in% c("vegdesc","vegtypecleartext") )
                 )#end IsAuthor
      Value[IsAuthor & ( Value %in% "dry sclerophyll forest;")] = "sub-tropical dry forest"
      Value[IsAuthor & ( Value %in% "heathland;"             )] = "sub-tropical scrubland"
      Value[IsAuthor & ( Value %in% "rainforest ;"           )] = "sub-tropical moist forest"
      Value[IsAuthor & ( Value %in% "shrubland;"             )] = "sub-tropical scrubland"
      Value[IsAuthor & ( Value %in% "tall shrubland;"        )] = "sub-tropical scrubland"
      Value[IsAuthor & ( Value %in% "wet sclerophyll forest;")] = "sub-tropical moist forest"
      Value[IsAuthor & ( Value %in% "woodland;"              )] = "sub-tropical savannah"
      #---~---


      #---~---
      #   Discard classes that are not easily classified to any of the main categories.
      #---~---
      Discard = 
         ( ( ! grepl(pattern="[a-z]",x=Value) )
         | ( Value %in% c( "???", "abandoned area", "abandoned field"
                         , "arable fields and gardens"
                         , "arable; broad-leaved woodland; coniferous plantation; grassland; heathland; wetland"
                         , "arable; riverine habitats; xerophytic shrubland; xerophytic woodland"
                         , "boreal river bank"
                         , "broad-leaved woodland or montane grassland or montane dwarf scrub"
                         , "coastal districts/river valleys", "coastal forest?"
                         , "dune grasslands plus a hay meadow", "fallow lands"
                         , "forest clearing", "forest community"
                         , "forest edges and related ecotones", "gravel-slide community"
                         , "half graval-slide community", "low alpine moderate snowbed", "ns"
                         , "other", "riparian corridor amist prairie grass", "riparian forest"
                         , "riparian meadow", "sandhills subclimax", "sea-shores"
                         , "secondary deciduous broad-leaved forest"
                         , "subalpine communities", "temperate disturbed"
                         , "temperate grassland; woodland; shrubland"
                         , "temperate grassland/woodland"
                         , "temperate river floodplain - deciduous with some conifer"
                         , "tropical coastal", "var", "varia", "various"
                         )#end c
           )#end Value
         )#end Discard
      Value[Discard] = NA_character_
      Valid[Discard] = FALSE
      VName[Discard] = NA_character_
      #---~---


      #---~---
      #   We standardise the output for a few classes
      #---~---
      IsTundra       =
         Value %in% c( "arctic", "alpine", "alpine bare patches amid geum turf"
                     , "alpine tundra", "alpine_fen", "alpine_lichen_heath", "alpine meadow"
                     , "arctic-montane", "boreal-tundra", "boreo-arctic montane"
                     , "mountain tundra", "snow_bed", "snowbed vegetation", "taiga"
                     , "tundra", "tundra (tussock-sedge; dwarf-shrub; moss tundra)"
                     , "wet tundra"
                     )#end c
      IsBorealFor    =
         Value %in% c( "boreal deciduous forest", "boreal forest"
                     , "boreal mixed forest", "boreal-montane", "boreal needleleaf forest"
                     , "boreo-temperate", "forest tundra"
                     , "larix forest with pinus and rich forb cover"
                     , "regenerated pinus sylvestris stands", "wide-boreal"
                     )#end c
      IsBorealScrub =
         Value %in% c( "boreal scrubland", "scrubs; alpine", "steppe; alpine"
                     , "open subnival vegetation"
                     , "tundra with evergreen dwarf-shrub and moss on high mountains in temperate zone"
                     )#end c
      IsBorealGrass =
         Value %in% c( "alpine meadow grassland", "boreal grassland", "canadian prairie"
                     , "canadian prairies", "meadow; alpine"
                     , "stunted heath on glacial moraine"
                     )#end c
      IsBorealWood   = Value %in% c( "boreal woodland")
      IsTempFor      =
         Value %in% c( "abies_forest", "acer forest", "barrens", "barrens community"
                     , "beech (fagus) forest", "beech-maple forest", "betula_forest"
                     , "birch (betula) forest", "birch (betula) forest with forbs"
                     , "birch (betula) forest with rhododendron"
                     , "broadleaf deciduous forest in temperate zone"
                     , "broadleaf deciduous forest in temperate zone: populus  davidiana and betula  platyphylla forest"
                     , "broad-leaved korean pine forest", "cool temperate rainforest"
                     , "cool-temperate rainforest", "coniferous forest", "deciduous forest"
                     , "deciduous species", "desciduous forest"
                     , "diverse; low for. w/ kanuka; totara; bleafs; vines"
                     , "evergreen temperate greenforests"
                     , "kahikatea for. w/ broadleaf understory; suburban"
                     , "low alpine willow veg", "lowland forest to subalpine scrub"
                     , "lowland to montane forest", "maritime pine forest"
                     , "mesic hemlock-step moss ecosystems"
                     , "microphyllous deciduous forest on mountains in temperate and subtropical zone"
                     , "mixed broadleaf deciduous and needleleaf evergreen forest in temperate zone"
                     , "mixed broadleaf deciduous and needleleaf forest in temperate zone"
                     , "mixed evergreen oak - temperate", "mixed forests"
                     , "mixed mesophytic forest", "mixed mountain and highland ecosystems"
                     , "mixed oak forest", "mixed podocarp for."
                     , "mixed podocarp for. w/ diverse broadlf understory"
                     , "mixed podocarp forest"
                     , "mixed podocarp/alectryon for.;  dense midstory"
                     , "mixed temperate deciduous forest", "montane conifer forest"
                     , "montane sclerophyll forest; chile", "montane; temperate"
                     , "mountain beech forest w/ depaup floor", "mountain forest"
                     , "mountain lowland complex with forests in the river valley"
                     , "mountain/red beech for. with depaup us"
                     , "mtn beech for. w/ shrubs and mtn beech regen"
                     , "mtn/silver beech for. w/ depaup us"
                     , "needleleaf evergreen forest in temperate zone"
                     , "needleleaf forest in cold-temperate or on mountains in temperate zone"
                     , "needleleaf forest on mountains in temperate zone"
                     , "oak (quercus) forest", "old growth temperate deciduous forest"
                     , "open kahikatea overstory w/ kamihi and tree ferns"
                     , "picea forest with betula; abies and forbs"
                     , "pine barrens of coastal plain", "pine (pinus) forest"
                     , "pinus_forest", "poplar (populus) forest"
                     , "populus forest with forbs"
                     , "quercetum herbosum; fagetum nudum and abiegnetum pteridiosum communities"
                     , "rimu/kamihi for. w/ shrubs; ferns; epis"
                     , "rimu/mattai/kamihi for. with diverse us"
                     , "short sohpora/hoheria for. w/ openings; vines"
                     , "silver beech for. w/ tall spindly kanuka; open us"
                     , "silver beech/ kamihi for. w/ vines and ferns"
                     , "silver beech/kamihi for. w/ diverse understory"
                     , "silver beech/kamihi for. w/ gaps and ferns"
                     , "silver beech/kamihi for. w/ferns + epis"
                     , "silver/mtn beech for. w/ open us and much moss"
                     , "silver/mtn beech for.; depaup underst w/ mb regen"
                     , "southern-temperate", "subalpine coniferous foret"
                     , "subalpine betula ermanii forest", "spruce (picea) forests"
                     , "spruce (picea) swamp forests"
                     , "tall forest; mixed podocarp and beech (beautiful)"
                     , "tall mixed broadleaf forest; suburban"
                     , "tall red beech for.  some shrubs"
                     , "tall red beech for. w/ open understory (gorgeous)"
                     , "tall red beech for.; open us"
                     , "tall; closed red/silver beech for. w/ dense shrubs"
                     , "temperate", "temperate broadleaf deciduous forest"
                     , "temperate broad-leaf forest", "temperate broadleaf forest"
                     , "temperate broadleaf forest; montane forest", "temperate coniferous"
                     , "temperate coniferous forest", "temperate decidous forest"
                     , "temperate deciduous broadleaf forest", "temperate deciduous forest"
                     , "temperate evergreen forest", "temperate evergreen needleleaf forest"
                     , "temperate forest", "temperate forest: picea  asperata forest"
                     , "temperate forest; slightly mediterranean"
                     , "temperate hardwood forest", "temperate mixed forest"
                     , "temperate montane;", "temperate needle leafed evergreen forest"
                     , "temperate needleleaf forest", "temperate rainforest"
                     , "temperate rain forest", "temperate seasonal forest"
                     , "temporate broad-leaf forest", "temporate hardwood forest"
                     , "virgin rimu/kamihi for. w/ tree ferns"
                     , "warm temperate deciduous broad-leaved forest", "wide-temperate"
                     )#end c
      IsTempWood     =
         Value %in% c( "dry forest; montane habitats", "forest/dry forest/steppe"
                     , "dry forest/steppe", "dry sclerophyll forest", "forest steppe"
                     , "microphyllous deciduous woodland along riverside in temperate zone"
                     , "open eucalypt woodland/savanna", "open sclerophyll woodland"
                     , "savanna and woodland", "savanna parkland", "sclerophyll woodland"
                     , "small birch (betula)  forest patches in steppe"
                     , "temperate deciduous woodland", "temperate grassy woodland"
                     , "temperate open woodlands", "temperate woodland"
                     , "temperate woodlands", "woodland/shruland"
                     )#end c
      IsTempScrub    =
         Value %in% c( "alpine steppe in temperate and subtropical zone"
                     , "broadleaf decidous scrub in temperate zone"
                     , "broadleaf deciduous scrub in temperate or subtropical zone"
                     , "broadleaf deciduous scrub in temperate zone: caragana microphylla var. daurica; salix spp.; art ..."
                     , "broadleaf deciduous scrub in temperate zone: tamarix spp. scrub on arid saline meadow soil"
                     , "deciduous scrub", "dense kanuka scrub with mossy floor"
                     , "dry heath - cairngorms (abandoned)"
                     , "dry heath- dorback (abandoned)", "dry heathland"
                     , "low sclerophyllous scrub-heath"
                     , "kanuka shrubland with grass and moss openings"
                     , "magellanic patagonian steppe"
                     , "nanophyton erinaceum dominated vegetation"
                     , "open communities within steppe zone"
                     , "open vegetation on tertiary rock outcrop", "patagonia"
                     , "patagonian steppe", "pebble bank", "rhododendron_dominanted"
                     , "rhododendron shrubland", "sclerophyll shrubland"
                     , "sclerophyll shrubland/woodland"
                     , "sclerophyll shrubland/woodland plus higher nutrient forests"
                     , "scrubs; temperate", "shortgrass steppe", "shrub-steppe"
                     , "shrub-steppe; wyoming", "steppe", "steppe; typical"
                     , "stunted heath w/ dwarf + bog pine + ferns"
                     , "tall scrub: manuka; m. beech; 3 spp of scrub pine"
                     , "tallish; open manuka with sedges and ferns"
                     , "temperate dwarf-shrubby or dwarf semi-shrubby needlegrass steppe"
                     , "temperate open scrub", "temperate steppe", "temperate scrubland"
                     , "temperate shrubland", "thorn bushland", "true steppe"
                     , "very low manuka/epachris heath w/ ferns + sedges"
                     )#end c
      IsTempGrass   =
         Value %in% c( "abandoned meadow", "abandoned meadow (1545 m)"
                     , "alopecurus dominated meadow", "alpine and subalpine meadow"
                     , "alpine and subalpine meadow in temperate zone"
                     , "alpine and subalpine meadow; locally combined with scrub in temperate or subtropical zone"
                     , "anabasis salsa dominated vegetation"
                     , "arthratherum dominated grassland", "calcareous temperate grassland"
                     , "extensively managed meadow", "extensively managed meadow (1520 m)"
                     , "festuca_varia_grassland", "geranium_hedisarum_meadow"
                     , "grazed meadow", "grazed meadow  - dorback"
                     , "grazed meadow - ben lawers (pasture)", "high prairie"
                     , "intensively managed hay meadow", "intensively managed meadow"
                     , "intensively managed meadow (1565 m)", "little used grasslands"
                     , "meadow; temperate", "mesotrophic grassland"
                     , "northern mixed prairie", "oligotrophic heath-grassland"
                     , "open subalpine meadow", "polygonum dominated meadow", "prairie"
                     , "prairie-chaparral transition"
                     , "prarie with planted and native trees", "priarie posholes"
                     , "permanent grassland", "shortgrass prairie", "steppe; meadow"
                     , "subalpine_grassland", "subalpine meadow", "temperate fields"
                     , "temperate grassland", "temperate grassland (grazed)"
                     , "temperate grassland with scattered shrubs/trees"
                     , "temperate grassland/herbfield", "temperate grasslands"
                     , "temperate grasslands and open scrub"
                     , "temperate grasslands and open woodlands"
                     , "temperate grasslands/herbfields/disturbed sites"
                     , "temperate grasslands/herbfields/ruderal habitats"
                     , "temperate herbfield", "temperate meadow"
                     , "temperate meadow: graminoid; forb meadow"
                     , "temperate meadow: iris lactea meadow"
                     , "temperate needlegrass steppe"
                     , "temperate needlegrass steppe: stipa grandis steppe"
                     , "temperate needlegrass steppe: stipa krylovii steppe"
                     , "temperate riparian meadows", "temporate grassland"
                     , "traditionally managed hay meadow"
                     , "ungrazed - ben lawers (abandoned)", "ungrazed meadow (abandoned)"
                     , "vegetation dominated by prangos pabularia and ferula kokanica"
                     )#end c
      IsSubTMoFor    =
         Value %in% c( "araucaria forest and campos grassland"
                     , "bamboo evergreen forest in subtropical zone"
                     , "broadleaf evergreen forest in subtropical zone", "cypress pine"
                     , "laurel forest", "laurisilva"
                     , "mixed broadleaf deciduous and evergreen forest on acid yellow-brown soil in subtropical zone"
                     , "mixed broadleaf deciduous and evergreen forest on limestone soil in subtropical zone"
                     , "montane atlantic rainforest; early secondary forest"
                     , "montane atlantic rainforest; late secondary forest"
                     , "needleleaf evergreen forest on mountains in subtropical or tropical zone"
                     , "needleleaf evergreen forest on mountains in subtropical zone"
                     , "subtropical evergreen broad-leaved forest"
                     , "subtropic evergreen forest", "subtropical everg broad-leaved forest"
                     , "subtropical evergreen broadleaf forest"
                     , "subtropical evergreen monsoon forest"
                     , "subtropical evergreen needleleaf forest"
                     , "subtropical floodplain forest", "subtropical forest"
                     , "subtropical forest in a context of forest-grassland mosaic"
                     , "subtropical mixed forest", "subtropical moist forest"
                     , "subtropical moist forest on karst", "subtropical monsoon forest"
                     , "subtropical wet forest", "sub-tropical moist forest"
                     , "sub-tropical forest", "tropical (atlantic) rain forest"
                     )#end c
      IsSubTDrFor    =
         Value %in% c( "sclerophyll (heathy) forest"
                     , "subtropical decidous broadleaf forest"
                     , "subtropical deciduous broadleaf forest"
                     , "subtropical eucalypt forest", "sub-tropical dry forest"
                     )#end c
      IsMediterran   =
         Value %in% c( "apennine high ecosystems"
                     , "broadleaf evergreen hemi-sclerophyllous thicket (shrubland) on mountiains in subtropical zone"
                     , "broad-sclerophyll forest", "california chaparral", "chaparral"
                     , "dehsa (wald-weide)", "dry mediterranean"
                     , "dry mediterranean shrub steppe", "fynbos shrubland", "garigue"
                     , "kwongan; mediterranean type ecosystem", "mallee", "mallee;"
                     , "mediterra forest", "mediterra shrubland", "mediterra woodland"
                     , "mediterranean", "mediterranean-atlantic", "mediterranean dry forest"
                     , "mediterranean evergreen species"
                     , "mediterranean grassland/herbfield", "mediterranean maquis"
                     , "mediterranean sclerophyll forest/ shrubland", "mediterranean scrub"
                     , "mediterranean shrubland", "mediterranean shrubland"
                     , "mediterranean wood land", "mediterranean woodland"
                     , "mediterranean; chilean matorral", "mediterranean; fynbos of cape"
                     , "mountain fynbos; mediterranean", "oak grass savanna"
                     , "redwood-forest undergrowth", "sclerophyuous scrub-heath (kwongan)"
                     , "subhumid mediterranean", "temperate oak savanna"
                     , "temperate sclerophyll forest - jarah forest"
                     , "woody scrub; classified as kamiesberg granite fynbos"
                     )#end c
      IsSubTropScrub = 
         Value %in% c( "evergreen sclerophyllous forest/shrubland"
                     , "grassland with emergent shrubs"
                     , "mixed broadleaf evergreen and deciduous scrub on acid soil in subtropical zone"
                     , "subtropical scrub forest", "sub-tropical scrubland"
                     , "sub-tropical shrubland"
                     )#end c
      IsSubTropSav   =
         Value %in% c( "sub-tropical savannah", "well-wooded savanna"
                     )#end c
      IsSubTropGrass =
         Value %in% c( "grassland near to a forest edge", "mediterranean grassland"
                     , "southern mixed prairie"
                     , "subtropical and temperate meadow along littoral: halophytic grass and forb meadow"
                     , "sub tropical grasslands", "sub-tropical grassland"
                     , "tallgrass prairie"
                     )#end c
      IsTropMoFor    =
         Value %in% c( "afromontane forest", "amazon caatinga (tropical rain forest)"
                     , "amazonian caatinga", "amazon forest", "bana'"
                     , "evergreen tropical moist lowland forest"
                     , "forest in ethiopian highlands"
                     , "lowland tropical moist semi-evergreen forest"
                     , "lower montane rain forest", "lowland tropical rain forest"
                     , "lowland tropical rainforest"
                     , "lowland evergreen tropical rain-forest"
                     , "moist tropical lowland forest"
                     , "monsoon evergreen broad-leaved forest", "montane cloud forest"
                     , "montane tropical rainforest", "near jambi; s. sumatra"
                     , "near kenangan; balikpapan", "premontane wet forest"
                     , "secondary growth tropical rainforest"
                     , "semi-deciduous tropical rain forest"
                     , "semi-deciduous tropical rain forest; more evergreen"
                     , "semi-deciduous tropical rainforest"
                     , "tropical broadleaf evergeen rain forest"
                     , "tropical broadleaf semi-evergreen forest (tropical monsoon forest)"
                     , "tropical coniferous forest", "tropical elfin forest"
                     , "tropical evergreen broadleaf forest"
                     , "tropical evergreen needleleaf forest", "tropical forest"
                     , "tropical floodplain forest", "tropical hevea  brasiliensis forest"
                     , "tropical humid forest"
                     , "tropical humid forests; plus some tropical deciduous forest"
                     , "tropical lowland evergreen and semi-evergreen forest (schmid; 1974; in blanc; 1998)"
                     , "tropical mixed hardwood forest", "tropical moist forest"
                     , "tropical mosit forest", "tropical montane forest"
                     , "tropical montane rain forest", "tropical mountain forest"
                     , "tropical mountain rain forest", "tropical premontane wet forest"
                     , "tropical rain forest"
                     , "tropical rain forest (premontane wet forest)"
                     , "tropical rain-forest", "tropical rainfores", "tropical rainforest"
                     , "tropical rainforest; ne queensland", "tropical secondary forest"
                     , "tropical wet forest", "tfrf_lw", "tfrf_up", "wet tropical forest"
                     )#end c
      IsTropDrFor    =
         Value %in% c( "caatinga (savanna)", "lowland tropical dry deciduous forest"
                     , "seasonal-dry tropical forest", "seasonally tropical dry forest"
                     , "semi-deciduous tropical forest", "sub-tropical dry forest"
                     , "subtropical dry forest", "trop heath forest"
                     , "tropical casuarina equisetifolia forest"
                     , "tropical deciduous broadleaf forest", "tropical decisuous forest"
                     , "tropical dry forest", "tropical seasonal forest"
                     , "tropical eucalypt open forest"
                     , "tropical seasonal semi-deciduous forest"
                     , "tropical semi-deceduous forest"
                     )#end c
      IsTropSav      =
         Value %in% c( "campo cerrado (tree savanna)", "cerrado (grass/tree savanna)"
                     , "cerrado (savanna)", "grass savanna", "grass/tree savanna"
                     , "mesic savanna", "sa", "sudano-sahelian savanna", "tree savana"
                     , "tree savanna", "tree savanna / subtropical dry forest"
                     , "tree savanna; sahel", "tropical savanna"
                     , "tropical savanna / nonsoon", "tropical savannah"
                     , "w african humid savanna"
                     )#end c
      IsTropScrub    =
         Value %in% c( "broadleaf evergreen and decidous scrub in tropical zone"
                     , "broadleaf evergreen scrub in tropical zone"
                     , "mixed broadleaf evergreen and deciduous scrub on acid soil in tropical zone"
                     , "caatinga (dry bush)", "semi-arid savanna", "tropical scrubland"
                     , "tropical shrubland"
                     )#end c
      IsTropGrass    =
         Value %in% c( "tropical and subtropical meadow", "tropical herbosa"
                     , "tropical grassland"
                     )#end c
      IsDesert       =
         Value %in% c( "arid", "arid desert", "arid desert; sahara"
                     , "arid valleys of the us sw", "atlas sahara", "desert"
                     , "desert (oasis)", "desert grass/dwarf shrub", "desert grasslands"
                     , "desert oasis", "desert sand dunes", "desert scrub"
                     , "desert vegetation"
                     , "desert vegetation dominated by ammodendron argenteum and henningia anisoptera"
                     , "dry rocks", "dry steppe", "early-successional desert vegetation"
                     , "ecotone between dwarf leaf succulent shrubs and grass"
                     , "ecotone: xeric shrub land / dry steppe"
                     , "ephemeral desert vegetation"
                     , "ephemeral desert vegetation (climax phase)"
                     , "ephemeral salt-tolerant vegetation"
                     , "haloxylon dominated desert woodland", "monte"
                     , "monte (native to tropical europe)", "monte?", "monte/semi-desert"
                     , "monte/semi-desert/steppe", "monte/steppe", "nw sonoran desert"
                     , "perennial desert shrubs", "poa bulbosa dominated desert vegetation"
                     , "riverine shrubbery in desert (russian: tugay)", "saharian desert"
                     , "salsola gemmascens vegetation", "semi-arid desert"
                     , "semi-arid grassland/shrubland", "semi-arid pasture"
                     , "semi-arid shrub-steppe", "semi-arid thrub-steppe", "semiarid desert"
                     , "semi desert/steppe", "semi-desert", "semi-desert/monte"
                     , "semi-desert/steppe", "shrub dominated desert vegetation"
                     , "sonoran desert", "steppe; desert", "steppe/semi-desert"
                     , "temperate deciduous dwarf semi-shrubby desert"
                     , "temperate forb-grass steppe (mesoxerophytic steppe) and xeromesophytic meadow"
                     , "temperate forb-grass steppe (mesoxerophytic steppe) and xeromesophytic meadow: aneurolepidium   ..."
                     , "temperate forb-grass steppe (mesoxerophytic steppe) and xeromesophytic meadow: festuca steppe"
                     , "temperate forb-grass steppe (mesoxerophytic steppe) and xeromesophytic meadow: filifolium sibir ..."
                     , "temperate semi-arboreous desert or saksaoul community"
                     , "temperate shrubby and semi-shrubby desert"
                     , "temperate succulent halophytic dwarf semi-shrubby desert"
                     , "warm deserts and semi-deserts", "xeric shrubland"
                     , "xeric shrubland; namaqualand broken veld"
                     )#end c
      IsMangrove     =
         Value %in% c( "broadleaf evergreen sclerophyllous forest or scrub along littoral in tropical zone (mangrove)"
                     , "mangrove", "mangrove forest"
                     )#end c
      IsWetlands     =
         Value %in% c( "aquatic", "bog", "bog/celery pine shrubland"
                     , "bog pine heath with mossy openings; herbs under"
                     , "calcarous oligotrophic dune wetland", "dry salt-marsh", "marsh"
                     , "mediterranean; wetland", "meadows and wetlands", "flooding pampa"
                     , "floodplain", "floodplain meadow"
                     , "manuka/bog pine flats w/ many shrubs and herbs"
                     , "salt marsh plant species", "salt water"
                     , "swamp meadow in termperate zone", "swamp forest", "shrub-bog"
                     , "temperate graminoid swamp"
                     , "temperate meadow: halophytic grass and forb meadow"
                     , "temperate swamp", "wet heath", "wet heathland", "wet meadow"
                     , "wet meadow; sweden", "wetland (1540 m)", "wetland", "wetlands"
                     , "wet_screens", "wet meadow", "wet meadows - molinion"
                     )#end c
      IsPasture      =
         Value %in% c( "commercial grasslands", "managed pasture", "pasture"
                     , "pasture (1565 m)", "subalpine mountain pasture"
                     )#end c
      IsCropland     =
         Value %in% c( "agricultural", "crop land", "cropland"
                     , "cropland; marginal land; ruderal habitats; gardens", "cultivated"
                     , "experimental fields", "medicago cropland"
                     , "orchards and rice fields"
                     )#end c
      IsPlantation   =
         Value %in% c( "apple tree orchards", "agrisiviculture: trees incropped"
                     , "artificial coniferous forest", "botanical garden"
                     , "drained plantation", "mediterran pine plantation", "plantation"
                     , "plantation on ploughed savanna", "plantation on upland heath"
                     , "restored forest"
                     )#end c
      #---~---



      #---~---
      #   Assign classes
      #---~---
      Value[IsDesert      ] = "01 - Desert/Semi-arid"
      Value[IsTropGrass   ] = "02 - Tropical Grassland"
      Value[IsTropScrub   ] = "03 - Tropical Scrubland"
      Value[IsTropSav     ] = "04 - Tropical Savannah"
      Value[IsTropDrFor   ] = "05 - Tropical Dry Forest"
      Value[IsTropMoFor   ] = "06 - Tropical Moist Forest"
      Value[IsMangrove    ] = "07 - Mangroves"
      Value[IsSubTropGrass] = "08 - Sub-tropical Grassland"
      Value[IsSubTropScrub] = "09 - Sub-tropical Scrubland"
      Value[IsSubTropSav  ] = "10 - Sub-tropical Savannah"
      Value[IsMediterran  ] = "11 - Mediterranean Ecosystems"
      Value[IsSubTDrFor   ] = "12 - Sub-tropical Dry Forest"
      Value[IsSubTMoFor   ] = "13 - Sub-tropical Moist Forest"
      Value[IsTempGrass   ] = "14 - Temperate Grasslands; Prairies"
      Value[IsTempScrub   ] = "15 - Temperate Scrublands; Steppe"
      Value[IsTempWood    ] = "16 - Temperate Woodlands"
      Value[IsTempFor     ] = "17 - Temperate Forests"
      Value[IsBorealGrass ] = "18 - Boreal Grasslands"
      Value[IsBorealScrub ] = "19 - Boreal Scrublands"
      Value[IsBorealWood  ] = "20 - Boreal Woodlands"
      Value[IsBorealFor   ] = "21 - Boreal Forests"
      Value[IsTundra      ] = "22 - Tundra; Alpine Ecosystems"
      Value[IsWetlands    ] = "23 - Wetlands; Bogs; Marshes"
      Value[IsPasture     ] = "24 - Pastures"
      Value[IsCropland    ] = "25 - Croplands"
      Value[IsPlantation  ] = "26 - Planted Forests"
      #---~---


      #---~---
      #   In case some class has not been accounted for, set data to NA but keep it valid,
      # so the code crashes and we can identify the missing classes.
      #---~---
      IsValid          = 
         ( Valid & ( IsDesert       | IsTropGrass    | IsTropScrub    | IsTropSav
                   | IsTropDrFor    | IsTropMoFor    | IsMangrove     | IsSubTropGrass
                   | IsSubTropScrub | IsSubTropSav   | IsMediterran   | IsSubTDrFor
                   | IsSubTMoFor    | IsTempGrass    | IsTempScrub    | IsTempWood
                   | IsTempFor      | IsBorealGrass  | IsBorealScrub  | IsBorealWood
                   | IsBorealFor    | IsTundra       | IsWetlands     | IsPasture
                   | IsCropland     | IsPlantation   )
         )#end IsValid
      Value[! IsValid] = NA_character_
      #---~---



      #---~---
      #   Standard names.
      #---~---
      OutAttribute = c("01 - Desert/Semi-arid","02 - Tropical Grassland"
                      ,"03 - Tropical Scrubland","04 - Tropical Savannah"
                      ,"05 - Tropical Dry Forest","06 - Tropical Moist Forest"
                      ,"07 - Mangroves","08 - Sub-tropical Grassland"
                      ,"09 - Sub-tropical Scrubland","10 - Sub-tropical Savannah"
                      ,"11 - Mediterranean Ecosystems","12 - Sub-tropical Dry Forest"
                      ,"13 - Sub-tropical Moist Forest"
                      ,"14 - Temperate Grasslands; Prairies"
                      ,"15 - Temperate Scrublands; Steppe","16 - Temperate Woodlands"
                      ,"17 - Temperate Forests","18 - Boreal Grasslands"
                      ,"19 - Boreal Scrublands","20 - Boreal Woodlands"
                      ,"21 - Boreal Forests","22 - Tundra; Alpine Ecosystems"
                      ,"23 - Wetlands; Bogs; Marshes","24 - Pastures","25 - Croplands"
                      ,"26 - Planted Forests"
                      )#end c
      #---~---
   }else if (AncilGrowth){
      #---~---
      #   Plant growth form attributed
      #---~---



      #---~---
      #   Make results case insensitive
      #---~---
      Value    = tolower(Value)
      #---~---


      #---~---
      #   For the main contributing author, this variable is more directly usable for 
      # the simple classification of growth form (TraitID 42) than the values listed in
      # the trait itself. Overwrite them.
      #---~---
      #   Kristine Engemann.  The standard form is frequently in the attributes.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Kristine Engemann" ) 
                 & ( NameOrig %in% "growthform_std" )
                 )#end IsAuthor
      TraitID[IsAuthor] = 42L
      Trait  [IsAuthor & (Value %in% "aquatic" )] = "Aquatic"
      Trait  [IsAuthor & (Value %in% "epiphyte")] = "Epiphyte"
      Trait  [IsAuthor & (Value %in% "herb"    )] = "Grass-Herb"
      Trait  [IsAuthor & (Value %in% "liana"   )] = "Liana"
      Trait  [IsAuthor & (Value %in% "parasite")] = "Parasite"
      Trait  [IsAuthor & (Value %in% "shrub   ")] = "Shrub"
      Trait  [IsAuthor & (Value %in% "tree    ")] = "Tree"
      Trait  [IsAuthor & (Value %in% "vine    ")] = "Vine"
      Value  [IsAuthor] = NA_character_
      Valid  [IsAuthor] = FALSE
      VName  [IsAuthor] = NA_character_
      #---~---



      #---~---
      #   Standard names.
      #---~---
      OutAttribute = c("Aquatic","Epiphyte","Grass-Herb","Liana","Parasite","Shrub","Tree"
                      ,"Vine"
                      )#end c
      #---~---

   }else if (AncilExperiment){
      #---~---
      #   Treatment precipitation addition/exclusion: Irrigation or drought
      #   Treatment
      #   Treatment light
      #   Treatment CO2
      #   Treatment: Exposition
      #   Treatment: Name of the experimental treatment
      #   Treatment P (phosphorus) supply
      #   Treatment conditions
      #   Treatment exposition: indoors / outdoors
      #---~---



      #---~---
      #   Note that instead of retaining the full information on experiments, the goal
      # of this harmonisation is to simply retain some basic categories of experiments,
      # instead of detailing the experiments. In all cases, the control data are clearly
      # separated from any treatment, because in most cases we would treat them as natural
      # samples.
      #
      # The current categories are currently accounted for:
      # 0 - control (of any type of experiment)
      # 1 - minor treatment (fence)
      # 2 - water treatment (well watered, drought)
      # 3 - light treatment (high light, shade)
      # 4 - nutrient treatment (fertilisation, suppression)
      # 5 - CO2 treatment (CO2 enrichment)
      # 6 - temperature treatment (warming, chilling)
      # 7 - other (anything else)
      #
      # Important: this classification relied mostly on the information provided by
      # authors. Some did not include details and are unpublished data sets. Whenever 
      # unsure, the experiments were assigned to the "Other" category". Likewise, compound
      # experiments (e.g., CO2 enrichment AND nutrient fertilisation) are assigned to 
      # other.
      #---~---



      #---~---
      #   Logical flags to identify which ancillary variable we are loading.
      #---~---
      IsTreatment        = DataID %in%  308L
      IsTreatWater       = DataID %in%  238L
      IsTreatLight       = DataID %in%  319L
      IsTreatCO2         = DataID %in%  324L
      IsTreatExpo        = DataID %in%  327L
      IsTreatPhosphorus  = DataID %in%  363L
      IsExperiment       = DataID %in%  490L
      IsTreatNitrogen    = DataID %in% 4052L
      IsInOutDoors       = DataID %in% 4695L
      #---~---



      #---~---
      #   Make results case insensitive. Also, convert Value to numeric, but suppress
      # warnings.
      #---~---
      Value    = tolower(Value)
      NumValue = suppressWarnings(as.numeric(Value))
      #---~---


      #---~---
      #   For all treatments, we standardise to the following classes.
      #---~---
      TreatStd = c( control     = "00 - Control"
                  , minor       = "01 - Minor"
                  , temperature = "02 - Temperature"
                  , water       = "03 - Water"
                  , light       = "04 - Light"
                  , co2         = "05 - CO2"
                  , nutrient    = "06 - Nutrient"
                  , other       = "07 - Other"
                  )#end c
      #---~---




      #---~---
      #   Remove data for the following authors as the data were numeric.
      #---~---
      IsAuthor         = 
         AuthorName %in% c( "Bradley Butterfield", "Johannes Cornelissen"
                          , "Eric Garnier", "Tucker Gilman", "Nate Hough-Snee"
                          , "Rebecca Montgomery", "Peter van Bodegom"
                          , "Mark van Kleunen")
      Value [IsAuthor & IsTreatLight] = NA_character_
      Valid [IsAuthor & IsTreatLight] = FALSE
      VName [IsAuthor & IsTreatLight] = NA_character_
      #---~---




      #---~---
      #   Nadejda Soudzilovskaia. Skip information on geography.
      #---~---
      IsAuthor = ( ( AuthorName %in% "Nadejda Soudzilovskaia" )
                 & IsTreatExpo
                 & ( NameOrig   %in% "geography"              ) )
      Value [IsAuthor] = NA_character_
      Valid [IsAuthor] = FALSE
      VName [IsAuthor] = NA_character_
      #---~---




      #---~---
      #   Set all values related to natural environment to "control", regardless of the 
      # author, when the information is provided in Data ID 327 (Treatment: Exposition).
      #---~---
      ValueNatural =
         c("canadian high arctic research station","control plot"
          ,"dry season measurements (see also #659)","field","field plants","forest stand"
          ,"forest trees","forest understorey","fully open overstory 90 days; seedling"
          ,"fully open overstory; seedling","fully sunlit - natural environment"
          ,"high desert","la selva biological station","in situ"
          ,"meadows (m) and pastures (p) on south east to south west exposed slopes"
          ,"montane meadow","mosses in forest"
          ,"nat env","natural","natural env","natural envireonment"
          ,"natural enviroment"
          ,"natural environment","natural environment (biodiversity experiment)"
          ,"natural environment; high regional n and s deposition"
          ,"natural environment; no warming; preccipitation ambient"
          ,"natural environment; sun exposed","natural-environment","natural_environment"
          ,"natural envrionment"
          ,"natural forest","natural forest environment","natural vegetation"
          ,"natural vegetation; but not top canopy","natural wetland environment"
          ,"natural wetlands (field conditions)","none","north facing slope","outdoor"
          ,"outdoor?","partially open overstory 90 days; seedling","saplings in forest"
          ,"siemianice","semi-native","shade - natural environment","south facing slope"
          ,"trees in field","undisturbed soil treatment; coniferous forest"
          ,"undisturbed soil treatment; deciduous alluvial forest"
          ,"undisturbed soil treatment; fallow wet meadow","undisturbed; seedling"
          )#end c
      Value[IsTreatExpo & (Value %in% ValueNatural)] = TreatStd["control"]
      #---~---




      #---~---
      #   Set all values related to non-natural environments but otherwise not active 
      # experiments, when the information is provided in Data ID 327 
      # (Treatment: Exposition).
      #---~---
      ValueMinor =
         c("alley","arboretum","arnold arboretum; harvard university","botanical garden"
          ,"botanical garden (bergius botanical garden; stockholm; sweden)"
          ,"botanical garden (bergius botanical garden; stockholm; sweden)"
          ,"botanical garden (royal botanical gardens; melbourne; victoria; australia)"
          ,"botanical garden (royal tasmanian botanical gardens; hobart; tasmania; australia)"
          ,"botanical garden (san francisco botanical garden; san francisco; ca; usa)"
          ,"botanical garden (the huntington library; art collections; and botanical gardens; pasadena; ca; usa)"
          ,"botanical garden (the jerusalem botanical gardens; jerusalem; israel)"
          ,"botanical garden (university of california botanical garden at berkeley; berkeley; ca; usa)"
          ,"campus of guangxi university","campus of yale university"
          ,"comm.garden.spain","common garden","common garden experiment"
          ,"common garden in growth containers with soil corresponding to seed origin"
          ,"common garden trees","common garden; understorey"
          ,"double strata agroforestry with pruned erythrina poeppigiana and chloroleucon eurycyclum"
          ,"double strata agroforestry with pruned erythrina poeppigiana and terminalia amazonia"
          ,"garden","grassland common garden"
          ,"grazing exclusion","heavy sheep-grazing with 8 sheep/ha stock rate"
          ,"growing with barley crop","growing without competing barley crop"
          ,"grown from seed sown in outdoor field experiment","intensive agriculture"
          ,"light sheep-grazing with 2 sheep/ha stock rate"
          ,"mature cultivated","moderate sheep-grazing with 4 sheep/ha stock rate"
          ,"monoculture","orchard","organic farming"
          ,"outdoor cultivation in 30x30cm pots; standardized substrate"
          ,"park","plantation","planted","planted mature trees/ shrubs"
          ,"planted near ghent in free air; high regional n and s deposition (30 kg n ha-1 yr-1; 20 kg s ha ..."
          ,"planted trees","planted vines"
          ,"simple agroforestry with chloroleucon eurycyclum"
          ,"simple agroforestry with chloroleucon eurycyclum + coffee"
          ,"simple agroforestry with pruned erythrina poeppigiana"
          ,"simple agroforestry with pruned erythrina poeppigiana + coffee"
          ,"simple agroforestry with terminalia amazonia"
          ,"simple agroforestry with terminalia amazonia + coffee","university campus"
          ,"university of california botanical garden","urban environmente","urban park"
          )#end c
      Value[IsTreatExpo & (Value %in% ValueMinor)] = TreatStd["minor"]
      #---~---




      #---~---
      #   Set all values related to highly controlled environments such as glass houses
      # and chambers to "Other", as they are likely very artificial conditions. We use
      # the generic approach only when the information is provided in Data ID 327 
      # (Treatment: Exposition).
      #---~---
      ValueOther =
         c("branch bag","chamber","climate chamber","climate chambers"
          ,"controlled climate chamber","controlled environment"
          ,"controlled environment room","cultivated botanical collection"
          ,"disturbed soil treatment; coniferous forest"
          ,"disturbed soil treatment; deciduous alluvial forest"
          ,"disturbed soil treatment; fallow wet meadow"
          ,"experimental; glasshouse","glass house","glasshouse","glasshouse experiment"
          ,"glasshouse; plastic tubes 10cm diameter; 100 cm depth","greehouse"
          ,"green house","greenhouse","greenhouse experiment","greenhouse plants"
          ,"greenhouse; grrowth container","greenhouse; indiana university"
          ,"greenhouse: highlight_highph_competition"
          ,"greenhouse: highlight_highph_nocompetition"
          ,"greenhouse: highlight_lowph_competition"
          ,"greenhouse: highlight_lowph_nocompetition"
          ,"greenhouse: lowleight_lowph_competition"
          ,"greenhouse: lowlight_highph_competition"
          ,"greenhouse: lowlight_highph_nocompetition"
          ,"greenhouse: lowlight_lowph_nocompetition","groth chamber","growth chamber"
          ,"growth chamber; -n","growth chamber; +n","growth chambers","growth exp"
          ,"growth_chamber","growth-chamber","herbarium","highly managed","houseplant"
          ,"hydroponic","lab"
          ,"large potted monoculture mesocosms; outside; soil from local meadow;  grazing and harvests emulated"
          ,"mesocosm","mini-ecosystem","nursery","open top","open top chambers"
          ,"open-sided growth chamber","open-top chamber","open-top chambers"
          ,"outdoor cultivations (4 repititions) grown in 30x30cm pots with standardized substrate. all pot ..."
          ,"pot","pot exp","pot-grown"
          ,"pots; outside in natural environment"
          ,"whole tree chambers","whole-tree chamber"
          )#end c
      Value[IsTreatExpo & (Value %in% ValueOther)] = TreatStd["other"]
      #---~---



      #---~---
      #   Christian Ammer.  Assume both dry and moist are water treatments. Light 
      # experiments have percent of open field light availability. Apply 
      # this to sun/shade.
      #---~---
      #--- Experimental. These are redundant with other information.
      IsAuthor = ( AuthorName %in% "Christian Ammer" ) & IsTreatExpo
      Value [IsAuthor] = NA_character_
      Valid [IsAuthor] = FALSE
      VName [IsAuthor] = NA_character_
      #--- Water
      IsAuthor = ( AuthorName %in% "Christian Ammer" ) & IsTreatWater
      Value[IsAuthor & ( Value %in% c("dry","moist") )] = TreatStd["water" ]
      #--- Light
      IsAuthor  = ( AuthorName %in% "Christian Ammer" ) & IsTreatLight
      IsShade   =  IsAuthor & ( NumValue %lt% 40       )
      IsPartial =  IsAuthor & ( NumValue %wr% c(41,85) )
      IsSunlit  =  IsAuthor & ( NumValue %gt% 85       )
      AncilID[IsShade | IsPartial | IsSunlit] = 443L
      Ancil  [IsShade  ] = "01 - Mostly Shaded"
      Ancil  [IsPartial] = "02 - Partially Shaded"
      Ancil  [IsSunlit ] = "03 - Mostly Sun-Exposed"
      Value  [IsShade | IsPartial | IsSunlit] = NA_character_
      Valid  [IsShade | IsPartial | IsSunlit] = FALSE
      VName  [IsShade | IsPartial | IsSunlit] = NA_character_
      #---~---



      #---~---
      #   Madhur Anand. Values indicate light availability. Otherwise, assign to other
      # because it is a pollution experiment.
      #---~---
      #--- Experiment
      IsAuthor = ( AuthorName %in% "Madhur Anand" ) & IsTreatExpo
      Value[IsAuthor & ( Value %in% "smelter polluted environment" ) ]  = TreatStd["other"]
      #--- Light
      IsAuthor         = ( AuthorName %in% "Madhur Anand" ) & IsTreatLight
      IsSunlit         =  IsAuthor & ( Value %in% "sun" )
      AncilID[IsSunlit] = 443L
      Ancil  [IsSunlit] = "03 - Mostly Sun-Exposed"
      Value  [IsSunlit] = NA_character_
      Valid  [IsSunlit] = FALSE
      VName  [IsSunlit] = NA_character_
      #---~---



      #---~---
      #   Michael J. Aspinwall.  Glasshouse
      #---~---
      #--- Experiment
      IsAuthor           = ( AuthorName %in% "Michael J. Aspinwall" ) & IsTreatExpo
      IsOtherExpo        = IsAuthor & ( Value %in% "glasshouse; climate controlled" )
      Value[IsOtherExpo] = TreatStd["other"]
      #---~---



      #---~---
      #   Owen Atkin.  Identify experiments. In addition, values indicate light level 
      # and CO2 level. Assume light and CO2 treatments if not assigned "other".
      #---~---
      #--- Experiment
      IsAuthor           = ( AuthorName %in% "Owen Atkin" ) & IsTreatExpo
      IsOtherExpo        = IsAuthor & ( Value %in% c("climate chamber","glasshouse") )
      Value[IsOtherExpo] = TreatStd["other"]
      #--- Light
      IsAuthor         = ( AuthorName %in% "Owen Atkin" ) & IsTreatLight
      Value[IsAuthor & ( Value %in% c("300","520") )] = TreatStd["light"]
      #--- CO2
      IsAuthor = ( AuthorName %in% "Owen Atkin" ) & IsTreatCO2
      Value[IsAuthor & ( Value %in% "350" )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "700" )] = TreatStd["co2"    ]
      #---~---



      #---~---
      #   Isabelle Aubin. 
      #---~---
      IsAuthor   = ( AuthorName %in% "Isabelle Aubin" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "control"                  )] = TreatStd["control" ]
      Value[IsAuthor & ( Value %in% c("mature","open" )        )] = TreatStd["minor"   ]
      Value[IsAuthor & ( Value %in% "full-tree /fertilization" )] = TreatStd["nutrient"]
      IsOther = 
         IsAuthor & ( Value %in% c( "full t blade compact", "full t trench"
                                  , "full tree blade", "full-tree no preparation"
                                  , "tlength trench", "peh", "tpe"
                                  )#end c
                    )#end Value
      Value[IsOther] = TreatStd["other" ]
      #---~---



      #---~---
      #   Kathleen Balazs.  Glasshouse or natural environment
      #---~---
      #--- Experiment
      IsAuthor           = ( AuthorName %in% "Kathleen Balazs" ) & IsTreatExpo
      IsOtherExpo        = IsAuthor & ( Value %in% "greenhouse - northern arizona university (standard controlled environment)" )
      Value[IsOtherExpo] = TreatStd["other"  ]
      #---~---



      #---~---
      #   Michael Beckmann.  Treatment is with UVB.
      #---~---
      #--- Experiment
      IsAuthor = ( AuthorName %in% "Michael Beckmann" ) & IsTreatExpo
      Value[IsAuthor & ( Value %in% "growth chamber" )] = TreatStd["other"   ]
      #--- UVB
      IsAuthor = ( AuthorName %in% "Michael Beckmann" ) & IsExperiment
      Value[IsAuthor & ( Value %in% "nouvb" )] = TreatStd["control" ]
      Value[IsAuthor & ( Value %in% "uvb"   )] = TreatStd["other"   ]
      #---~---



      #---~---
      #   Michael Belluau. Water stress 
      #---~---
      #--- Experiment
      IsAuthor    = ( AuthorName %in% "Michael Belluau" ) & IsTreatExpo
      IsWaterExpo = IsAuthor & ( Value %in% "water stress experiment" )
      IsOtherExpo = IsAuthor & ( Value %in% "climate chamber; non-limiting conditions; (cf. dataset reference)" )
      Value[IsWaterExpo] = TreatStd["water"]
      Value[IsOtherExpo] = TreatStd["other"]
      #---~---





      #---~---
      #   Joana Bergmann. 
      #---~---
      IsAuthor   = ( AuthorName %in% "Joana Bergmann" ) & IsTreatment
      Value[IsAuthor & grepl(pattern="2 ml of a microbial",x=Value)] = TreatStd["other"]
      #---~---



      #---~---
      #   Peter van Bodegom. They used a 0/1 code for the control/phosphorus treatment.
      #---~---
      IsAuthor = ( AuthorName %in% "Peter van Bodegom" ) & IsTreatPhosphorus
      Value[IsAuthor & ( Value %in% "0" )] = TreatStd["control" ]
      Value[IsAuthor & ( Value %in% "1" )] = TreatStd["nutrient"]
      #---~---



      #---~---
      #   Coline Boonman. Light experiment as they used lamps.
      #---~---
      IsAuthor = ( AuthorName %in% "Coline Boonman" ) & IsTreatLight
      IsSunlit = 
         ( IsAuthor 
         & ( Value %in% "daylight; supported by lamps during dark daytime conditions" )
         )#end IsAuthor
      Value [IsSunlit] = TreatStd["light"]
      #---~---



      #---~---
      #   Serra Willow Buchanan. Light experiment but not artifical lighting. Assign
      # results to the light conditions rather than light treatment.
      #---~---
      IsAuthor  = ( AuthorName %in% "Serra Willow Buchanan" ) & IsTreatLight
      IsShade   = IsAuthor & grepl(pattern="^permanent full shade;",x=Value)
      IsPartial = IsAuthor & grepl(pattern="^managed shade;"       ,x=Value)
      IsSunlit  = IsAuthor & grepl(pattern="^full sun monoculture" ,x=Value)
      AncilID[IsShade | IsPartial | IsSunlit] = 443L
      Ancil  [IsShade                       ] = "01 - Mostly Shaded"
      Ancil  [IsPartial                     ] = "02 - Partially Shaded"
      Ancil  [IsSunlit                      ] = "03 - Mostly Sun-Exposed"
      Value  [IsShade | IsPartial | IsSunlit] = NA_character_
      Valid  [IsShade | IsPartial | IsSunlit] = FALSE
      VName  [IsShade | IsPartial | IsSunlit] = NA_character_
      #---~---


      #---~---
      #   Marc Cadotte.  Experiment was seeding plots with a mix of species, but no
      # additional treatment. Set it to minor
      #---~---
      IsAuthor   = ( AuthorName %in% "Marc Cadotte" ) & IsTreatExpo
      IsMinorExp = Value %in% c("field experiment")
      Value[IsAuthor & IsMinorExp] = TreatStd["minor"]
      #---~---


      #---~---
      #   Michele Carbognani.  Warming experiment
      #---~---
      IsAuthor   = ( AuthorName %in% "Michele Carbognani" ) & IsTreatExpo
      IsWarmExp  = Value %in% c("experimental warming with open top chambers during the snow-free period; increasing daily mean  ...")
      Value[IsAuthor & IsWarmExp] = TreatStd["temperature"]
      #---~---


      #---~---
      #   Johannes Cornelissen.  Treatment is on irrigation.
      #---~---
      IsAuthor = ( AuthorName %in% "Johannes Cornelissen" ) & IsTreatWater
      Value[IsAuthor & ( Value %in% c("n")      )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% c("rehydr") )] = TreatStd["water"  ]
      #---~---


      #---~---
      #   Will Cornwell.  Treatment is on irrigation..
      #---~---
      IsAuthor = ( AuthorName %in% "Will Cornwell" ) & IsTreatWater
      Value[IsAuthor & ( Value %in% c("n") )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% c("y") )] = TreatStd["water"  ]
      #---~---



      #---~---
      #   Daniel Falster.  Set growing condition classes to treatment types.
      #
      # CG -- common garden
      # FE -- field experimental
      # FW -- field wild
      # GC -- growth chamber
      # GH -- glasshouse
      # PM -- plantation managed
      # PU -- plantation unmanaged
      #---~---
      IsAuthor   = ( AuthorName %in% "Daniel Falster" ) & IsTreatExpo
      Value[IsAuthor & ( Value %in% "cg")] = TreatStd["minor"  ]
      Value[IsAuthor & ( Value %in% "fe")] = TreatStd["other"  ]
      Value[IsAuthor & ( Value %in% "fw")] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "gc")] = TreatStd["other"  ]
      Value[IsAuthor & ( Value %in% "gh")] = TreatStd["other"  ]
      Value[IsAuthor & ( Value %in% "pm")] = TreatStd["minor"  ]
      Value[IsAuthor & ( Value %in% "pu")] = TreatStd["minor"  ]
      #---~---



      #---~---
      #   Fatih Fazlioglu. 
      #---~---
      IsAuthor   = ( AuthorName %in% "Fatih Fazlioglu" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "control"         )] = TreatStd["control" ]
      IsMinor    = Value %in% c("high competition", "low competition")
      Value[IsAuthor & IsMinor                         ] = TreatStd["minor"   ]
      Value[IsAuthor & ( Value %in% "shade treatment" )] = TreatStd["light"   ]
      Value[IsAuthor & ( Value %in% "low_ph"          )] = TreatStd["other"   ]
      #---~---



      #---~---
      #   Gregoire Freschet. Flag light experiment, and assign all nitrogen treatments
      # as treatments.
      #---~---
      #--- Light
      IsAuthor         = ( AuthorName %in% "Gregoire Freschet" ) & IsTreatLight
      Value [IsAuthor] = TreatStd["light"]
      #--- Nutrient
      IsAuthor = ( AuthorName %in% "Gregoire Freschet" ) & IsTreatNitrogen
      Value[IsAuthor & (Value %in% c("high soil n","low soil n") )] = TreatStd["control" ]
      #---~---



      #---~---
      #   Erin Garnier.  We don't assign abandonment or land use histories to any
      # treatment, unless there is indication of fertilisation (but we assign to other
      # treatment as this is not the typical controlled experiment).
      #---~---
      IsAuthor  = ( AuthorName %in% "Eric Garnier" ) & IsExperiment
      IsControl = 
         Value %in% c( "bal_abandoned >60 years", "bal_abandoned 15-60 years"
                     , "bal_abandoned 5-15 years", "bal_grazing", "hgm_abandoned 2 years"
                     , "hgm_abandoned 25-45 years", "hgm_abandoned 5-15 years"
                     , "kde_abandonment early", "kde_abandonment late", "kde_heavy grazing"
                     , "kde_moderate grazing", "lag_abandoned field grazed"
                     , "lag_dense shrubland grazed", "lag_grassland grazed"
                     , "lag_open shrubland grazed", "lau_never ploughed/grazed"
                     , "lau_never ploughed/mown", "lau_ploughed/grazed"
                     , "lau_ploughed/mown", "mer_abandoned 12 years"
                     , "mer_abandoned 20 years", "mer_grazing", "mnp_abandoned/dry"
                     , "mnp_abandoned/fresh", "mnp_high intensive grazing/dry"
                     , "mnp_high intensive grazing/fresh"
                     , "mnp_medium intensive grazing/dry"
                     , "mnp_medium intensive grazing/fresh"
                     , "nor_high continous grazing by sheep through summer"
                     , "nor_low continous grazing by sheep through summer"
                     , "ohr_mown/unfertilized", "ohr_unmown/unfertilized"
                     , "sut_abandoned fallow", "sut_unploughed"
                     )#end c
      IsOther = 
         Value %in% c( "erc_grazing/high nutrient", "erc_grazing/low nutrient"
                     , "erc_grazing/mowing/grazing/high nutrient"
                     , "erc_grazing/mowing/grazing/low nutrient"
                     , "erc_mowing/mowing/grazing/high nutrient"
                     , "erc_mowing/mowing/grazing/low nutrient"
                     , "lau_ploughed/mown and fertilized", "mer_rotational agriculture"
                     , "ohr_mown/fertilized", "ohr_unmown/fertilized"
                     , "sut_short rotation fallow"
                     )#end c
      Value[IsAuthor & IsControl] = TreatStd["control"]
      Value[IsAuthor & IsOther  ] = TreatStd["other"  ]
      #---~---


      #---~---
      #   Colleen Iversen.  Information on outdoor/indoor experiment. Assume indoor to
      # be other experiment. Otherwise, they provide details from experiment, ignore 
      # information.
      #---~---
      #--- Indoor/outdoor
      IsAuthor  = ( AuthorName %in% "Colleen Iversen" ) & IsInOutDoors
      Value[IsAuthor & ( Value %in% "outdoor")] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "indoor" )] = TreatStd["other"  ]
      #--- Details
      IsAuthor = ( ( AuthorName %in% "Colleen Iversen" )
                 & IsTreatment
                 & ( NameOrig %in% "notes_treatment extent" )
                 )#end IsAuthor
      Value  [IsAuthor] = NA_character_
      Valid  [IsAuthor] = FALSE
      VName  [IsAuthor] = NA_character_
      #---~---



      #---~---
      #   Steven Jansen.  Keys describe the growing environment of sampled plant: 
      # C  -- common garden
      # E  -- experiment
      # G  -- greenhouse
      # N  -- natural
      #---~---
      IsAuthor = ( AuthorName %in% "Steven Jansen" ) & IsTreatExpo
      Value[IsAuthor & ( Value %in% c("c") )] = TreatStd["minor"  ]
      Value[IsAuthor & ( Value %in% c("e") )] = TreatStd["other"  ]
      Value[IsAuthor & ( Value %in% c("g") )] = TreatStd["other"  ]
      Value[IsAuthor & ( Value %in% c("n") )] = TreatStd["control"]
      #---~---



      #---~---
      #   Jens Kattge. Light information of categories not provided.
      # Assign light for it but it may not have been light treatment in the strictest 
      # sense. CO2 treatment: assign control/treatment based on the CO2 levels.
      #---~---
      #--- Light
      IsAuthor = ( AuthorName %in% "Jens Kattge" ) & IsTreatLight
      Value[IsAuthor] = TreatStd["light"]
      #--- CO2
      IsAuthor     = ( AuthorName %in% "Jens Kattge" ) & IsTreatCO2
      IsCO2Control = Value %in% c("actual","ambient (about 360ppm)")
      IsCO2Expo    = Value %in% c("elevated","reduced")
      IsCO2Unknown = Value %in% "unknown"
      Value[IsAuthor & IsCO2Control] = TreatStd["control"]
      Value[IsAuthor & IsCO2Expo   ] = TreatStd["co2"    ]
      Value[IsAuthor & IsCO2Unknown] = NA_character_
      Valid[IsAuthor & IsCO2Unknown] = FALSE
      VName[IsAuthor & IsCO2Unknown] = NA_character_
      #--- Additional experiments
      IsAuthor     = ( AuthorName %in% "Jens Kattge" ) & IsTreatExpo
      IsWaterExpo  = Value %in% c("dry plot")
      IsCO2Expo    = Value %in% c("face")
      Value[IsAuthor & IsWaterExpo ] = TreatStd["water"]
      Value[IsAuthor & IsCO2Expo   ] = TreatStd["co2"  ]
      #---~---



      #---~---
      #   Dushan Kumarathunge. Assign control/treatment based on the CO2 levels.
      #---~---
      #--- CO2
      IsAuthor = ( AuthorName %in% "Dushan Kumarathunge" ) & IsTreatCO2
      Value[IsAuthor & ( Value %in% "ambient"  )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "elevated" )] = TreatStd["co2"    ]
      #--- Field
      IsAuthor = ( AuthorName %in% "Dushan Kumarathunge" ) & IsTreatExpo
      Value[IsAuthor & ( Value %in% "field (cg)" )] = TreatStd["minor"  ]
      Value[IsAuthor & ( Value %in% "field (ne)" )] = TreatStd["control"]
      #---~---



      #---~---
      #   Kim La Pierre.  Two variables were used for describing the experiment. Only
      # DataID 308 (Treatment) differentiates treatments.
      #---~---
      #--- Treatment
      IsAuthor   = ( AuthorName %in% "Kim La Pierre" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "x"     )] = TreatStd["control" ]
      Value[IsAuthor & ( Value %in% "fence" )] = TreatStd["minor"   ]
      IsNutrient = Value %in% c( "k", "n", "nk", "np", "npk", "npkfence", "p", "pk")
      Value[IsAuthor & IsNutrient            ] = TreatStd["nutrient"]
      #--- Treatment description. Ignore values
      IsAuthor   = ( ( AuthorName %in% "Kim La Pierre" )
                   & IsTreatExpo
                   & ( NameOrig   %in% "exposition"    )
                   )#end IsAuthor
      Value  [IsAuthor] = NA_character_
      Valid  [IsAuthor] = FALSE
      VName  [IsAuthor] = NA_character_
      #---~---



      #---~---
      #   Sandra Lavorel.  Keys describe the growing environment of sampled plant: 
      #---~---
      IsAuthor      = ( AuthorName %in% "Sandra Lavorel" ) & IsTreatExpo
      IsNatural     = 
         Value %in% c("bau-abandonee/seslerie non paturee."
                     ,"bau-abandonnee/nardaie non paturee"
                     ,"bau-abandonnee/nardaie reposoir"
                     ,"bau-abandonnee/prairie grasse non paturee"
                     ,"lau_never ploughed/grazed","lau_never ploughed/grazed highland"
                     ,"lau_never ploughed/grazed slopes","lau_never ploughed/mown"
                     ,"none","ver_never ploughed/grazed highland"
                     ,"ver_never ploughed/grazed slopes"
                     )#end c
      IsMinorExpo    = 
         Value %in% c("bau-nardaie paturee.","bau-paturee intermediaire grasse-seslerie"
                     ,"bau-prairie a seslerie paturee","bau-prairie grasse paturee"
                     ,"lau_grazed/abandoned","lau_ploughed/grazed","lau_ploughed/mown"
                     ,"ver_extensively grazed meadow"
                     ,"ver_permanent extensively mown meadow"
                     ,"ver_ploughed temporary grassland"
                     ,"ver_ski slope/ extensive summer grazing"
                     )#end c
      IsNutrientExpo = 
         Value %in% c("lau_ploughed/mown and fertilized"
                     ,"ver_permanent meadow mown and fertilized"
                     ,"ver_permanent meadows mown and fertilized")
      
      Value[IsAuthor & IsNatural     ] = TreatStd["control" ]
      Value[IsAuthor & IsMinorExpo   ] = TreatStd["minor"   ]
      Value[IsAuthor & IsNutrientExpo] = TreatStd["nutrient"]
      #---~---



      #---~---
      #   Yan-Shih Lin. All samples were flagged as "opt". The description says growing
      # under ambient or stressed conditions. Assigning minor, although many may be from
      # control. For CO2, assume all levels provided are control.
      #---~---
      #--- General
      IsAuthor   = ( AuthorName %in% "Yan-Shih Lin" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "opt" )] = TreatStd["minor"]
      #--- CO2
      IsAuthor = ( AuthorName %in% "Yan-Shih Lin" ) & IsTreatCO2
      Value[IsAuthor & ( Value %in% c("350","ambient","control")  )] = TreatStd["control"]
      #--- Miscellaneous
      IsAuthor = ( AuthorName %in% "Yan-Shih Lin" ) & IsTreatExpo
      Value[IsAuthor & ( Value %in% "face"     )] = TreatStd["co2"  ]
      Value[IsAuthor & ( Value %in% "otc"      )] = TreatStd["other"]
      Value[IsAuthor & ( Value %in% "otc field")] = TreatStd["other"]
      Value[IsAuthor & ( Value %in% "otc o3"   )] = TreatStd["other"]
      Value[IsAuthor & ( Value %in% "wtc"      )] = TreatStd["other"]
      #---~---



      #---~---
      #   Daijun Liu. Assuming other for AM because details on the experiment were not 
      # detailed
      # in the description
      #---~---
      IsAuthor = ( AuthorName %in% "Daijun Liu" ) & IsTreatment
      Value[IsAuthor & ( Value %in% c("none-am","none-am+none-drought" ) )] = 
                                                                     TreatStd["control" ]
      Value[IsAuthor & grepl(pattern="none-am\\+drought",x=Value)] = TreatStd["water"   ]
      Value[IsAuthor & grepl(pattern="^am"              ,x=Value)] = TreatStd["other"   ]
      #---~---


      #---~---
      #   Justin Luong. Rainfall manipulation experiments. According to the notes, shelter
      # means 60% rain exclusion.
      #---~---
      IsAuthor = ( AuthorName %in% "Justin Luong" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "control"              )] = TreatStd["control" ]
      Value[IsAuthor & grepl(pattern="^2l of water",x=Value)] = TreatStd["water"   ]
      Value[IsAuthor & ( Value %in% "60% rain exclusion"   )] = TreatStd["water"   ]
      Value[IsAuthor & ( Value %in% "shelter"              )] = TreatStd["water"   ]
      #--- Exposure. Ignore the information as it is defined by the other variable.
      IsAuthor = ( AuthorName %in% "Justin Luong" ) & IsTreatExpo
      Value  [IsAuthor] = NA_character_
      Valid  [IsAuthor] = FALSE
      VName  [IsAuthor] = NA_character_
      #---~---



      #---~---
      #   Peter Manning. CO2 levels are ambient.
      #---~---
      IsAuthor = ( AuthorName %in% "Peter Manning" ) & IsTreatCO2
      Value[IsAuthor & ( Value %in% c("360")  )] = TreatStd["control"]
      #---~---


      #---~---
      #   Mara McPartland. Water level experiments. Information is likely redundant
      #---~---
      #--- Variable treament
      IsAuthor = ( AuthorName %in% "Mara McPartland" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "control"          )] = TreatStd["control" ]
      Value[IsAuthor & grepl(pattern="lowered$",x=Value)] = TreatStd["water"   ]
      Value[IsAuthor & grepl(pattern="raised$" ,x=Value)] = TreatStd["water"   ]
      #--- Variable treatment: experiment
      IsAuthor = ( AuthorName %in% "Mara McPartland" ) & IsTreatExpo
      Value[IsAuthor & ( Value %in% "natural environment")] = TreatStd["control" ]
      Value[IsAuthor & ( Value %in% "drought treatment"  )] = TreatStd["water"   ]
      Value[IsAuthor & ( Value %in% "water treatment"    )] = TreatStd["water"   ]
      #---~---




      #---~---
      #   Belinda Medlyn. Assign control/treatment based on CO2 level.
      #---~---
      #--- CO2
      IsAuthor = ( AuthorName %in% "Belinda Medlyn" ) & IsTreatCO2
      Value[IsAuthor & ( Value %in% c("350","355","360","ambient"))] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% c("467","583","700","710"    ))] = TreatStd["co2"    ]
      #--- Experiment
      IsAuthor = ( ( AuthorName %in% "Belinda Medlyn" )
                 & IsTreatExpo
                 & ( NameOrig   %in% "type of experiment" )
                 )#end IsAuthor
      Value[IsAuthor & ( Value %in% "branch bag"               )] = TreatStd["other"   ]
      Value[IsAuthor & ( Value %in% "forest fertilization"     )] = TreatStd["nutrient"]
      Value[IsAuthor & ( Value %in% "forest stand"             )] = TreatStd["control" ]
      Value[IsAuthor & ( Value %in% "growth chamber"           )] = TreatStd["other"   ]
      Value[IsAuthor & ( Value %in% "mini-ecosystem"           )] = TreatStd["other"   ]
      Value[IsAuthor & ( Value %in% "open-sided growth chamber")] = TreatStd["other"   ]
      Value[IsAuthor & ( Value %in% "open-top chamber"         )] = TreatStd["other"   ]
      Value[IsAuthor & ( Value %in% "whole-tree chamber"       )] = TreatStd["other"   ]
      #--- Details
      IsAuthor = ( ( AuthorName %in% "Belinda Medlyn" )
                 & IsTreatExpo
                 & ( NameOrig %in% "growthcond" )
                 )#end IsAuthor
      Value  [IsAuthor] = NA_character_
      Valid  [IsAuthor] = FALSE
      VName  [IsAuthor] = NA_character_
      #---~---



      #---~---
      #   Patrick Meir. Light information of categories not provided.
      # Assign light for it but it may not have been light treatment in the strictest 
      # sense. For CO2, all levels seems to be ambient.
      #---~---
      #--- Light
      IsAuthor = ( AuthorName %in% "Patrick Meir" ) & IsTreatLight
      Value[IsAuthor] = TreatStd["light"]
      #--- CO2
      IsAuthor  = ( AuthorName %in% "Patrick Meir" ) & IsTreatCO2
      IsControl = Value %in% c("actual","ambient (about 360ppm)")
      Value[IsAuthor & IsControl] = TreatStd["control"]
      #---~---



      #---~---
      #   Sean Michaletz. Warming experiment.
      #---~---
      #--- Light
      IsAuthor   = ( AuthorName %in% "Sean Michaletz" ) & IsTreatExpo
      IsWarmExpo = Value %in% "harte warming experiment (montane meadow)"
      Value[IsAuthor & IsWarmExpo] = TreatStd["temperature"]
      #---~---


      #---~---
      #   Vanessa Minden. Nutrient and light experiments. 
      #---~---
      #--- Nutrient
      IsAuthor = ( AuthorName %in% "Vanessa Minden" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "control"              )] = TreatStd["control" ]
      Value[IsAuthor & ( Value %in% "balanced"             )] = TreatStd["other"   ]
      Value[IsAuthor & grepl(pattern="limitation$" ,x=Value)] = TreatStd["nutrient"]
      Value[IsAuthor & grepl(pattern="nitrogen"    ,x=Value)] = TreatStd["nutrient"]
      Value[IsAuthor & grepl(pattern="penicilin"   ,x=Value)] = TreatStd["other"   ]
      Value[IsAuthor & grepl(pattern="sulfadiazine",x=Value)] = TreatStd["other"   ]
      Value[IsAuthor & grepl(pattern="tetracycline",x=Value)] = TreatStd["other"   ]
      #--- Light
      IsAuthor = ( AuthorName %in% "Vanessa Minden" ) & IsTreatLight
      Value [IsAuthor & ( Value %in% "ambient light" )] = TreatStd["control"]
      Value [IsAuthor & ( Value %in% "low light"     )] = TreatStd["light"  ]
      Value [IsAuthor & ( Value %in% "medium light"  )] = TreatStd["light"  ]
      #---~---


      #---~---
      #   Rachel Mitchell. Drought and shade treatments.
      #---~---
      IsAuthor = ( AuthorName %in% "Rachel Mitchell" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "ambient" )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "drought" )] = TreatStd["water"  ]
      Value[IsAuthor & ( Value %in% "shade"   )] = TreatStd["light"  ]
      #---~---


      #---~---
      #   Christopher Muir.  Water treatment is on irrigation. They also have light
      # experiment.
      #---~---
      #--- Water
      IsAuthor = ( AuthorName %in% "Christopher Muir" ) & IsTreatWater
      Value[IsAuthor & ( Value %in% c("regular to preven drought") )] = TreatStd["water"]
      #--- Light
      IsAuthor = ( AuthorName %in% "Christopher Muir" ) & IsTreatLight
      Value [IsAuthor & ( Value %in% "supplementary light; 16:8 h" )] = TreatStd["light"]
      #---~---


      #---~---
      #   Ulo Niinemets.  Growth conditions provided. If Natural/chamber conditions,
      # assume natural based on the standard value string
      #---~---
      IsAuthor = ( ( AuthorName %in% "Ulo Niinemets" )
                 & IsTreatExpo
                 & ( NameOrig   %in% "artificial growth conditions (g=greenhouse; c=growth chamber)" )
                 )#end IsAuthor
      Value[IsAuthor & ( Value %in% "c"         )] = TreatStd["other"  ]
      Value[IsAuthor & ( Value %in% "g"         )] = TreatStd["other"  ]
      Value[IsAuthor & ( Value %in% "natural/c" )] = TreatStd["control"]
      #---~---



      #---~---
      #   Antonio Jesus Perea. Canopy position rather than light level.
      #---~---
      IsAuthor = ( AuthorName %in% "Antonio Jesus Perea" ) & IsTreatLight
      AncilID[IsAuthor] = 443L
      Ancil  [IsAuthor & ( Value %in% "shadow" )] = "01 - Mostly Shaded"
      Ancil  [IsAuthor & ( Value %in% "open"   )] = "03 - Mostly Sun-Exposed"
      Ancil  [IsAuthor & ( Value %in% "adult"  )] = "03 - Mostly Sun-Exposed"
      Value  [IsAuthor                          ] = NA_character_
      Valid  [IsAuthor                          ] = FALSE
      VName  [IsAuthor                          ] = NA_character_
      #---~---


      #---~---
      #   Giacomo Puglielli. All data are actually control data.
      #---~---
      IsAuthor = ( AuthorName %in% "Giacomo Puglielli" ) & IsTreatment
      Value[IsAuthor & grepl(pattern="^control",x=Value)] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "none"             )] = TreatStd["control"]
      #---~---



      #---~---
      #   Peter Reich. Treatment light available. For CO2, assign control/treament based
      # on the level.
      #---~---
      #--- Light
      IsAuthor         = ( AuthorName %in% "Peter Reich" ) & IsTreatLight
      Value [IsAuthor] = TreatStd["light"]
      #--- CO2
      IsAuthor = ( AuthorName %in% "Peter Reich" ) & IsTreatCO2
      Value[IsAuthor & ( Value %in% "ambient"  )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "elevated" )] = TreatStd["co2"    ]
      #--- Miscellaneous
      IsAuthor = ( AuthorName %in% "Peter Reich" ) & IsTreatExpo
      IsMisc   = Value %in% "i do not like this paper. results looks questionable"
      Value[IsAuthor & IsMisc ] = NA_character_
      Valid[IsAuthor & IsMisc ] = FALSE
      VName[IsAuthor & IsMisc ] = NA_character_
      #---~---



      #---~---
      #   Lawren Sack. The column treatment was used for multiple purposes, not 
      # necessarily for the idea of treatment. Assigning all of the samples to 
      # minor treatment because the samples came from common gardens.
      #---~---
      IsAuthor = ( AuthorName %in% "Lawren Sack" ) & IsTreatment
      Value[IsAuthor & ( ! is.na(Value) )] = TreatStd["minor"]
      #---~---


      #---~---
      #   Rob Salguero-Gomez. The column treatment is not very informative.
      #---~---
      IsAuthor = ( AuthorName %in% "Rob Salguero-Gomez" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "control"           )] = TreatStd["control" ]
      Value[IsAuthor & ( Value %in% "phosphorus"        )] = TreatStd["nutrient"]
      Value[IsAuthor & ( Value %in% "resprout"          )] = TreatStd["other"   ]
      #---~---


      #---~---
      #   Brody Sandel. Not entirely clear what sugar is, assigning to other treatment.
      #---~---
      IsAuthor = ( AuthorName %in% "Brody Sandel" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "control" )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "sugar"   )] = TreatStd["other"  ]
      #---~---


      #---~---
      #   Marina Scalon. Assign litter to minor treatment, control, and nutrient
      # treatments, and fire to other treatment.
      #---~---
      #--- Treatment
      IsAuthor = ( AuthorName %in% "Marina Scalon" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "biennial fire"     )] = TreatStd["other"   ]
      Value[IsAuthor & grepl(pattern="^ca\\+mg" ,x=Value)] = TreatStd["nutrient"]
      Value[IsAuthor & ( Value %in% "cerrado ss"        )] = TreatStd["control" ]
      Value[IsAuthor & grepl(pattern="^litter"  ,x=Value)] = TreatStd["minor"   ]
      Value[IsAuthor & grepl(pattern="^n\\ "    ,x=Value)] = TreatStd["nutrient"]
      Value[IsAuthor & grepl(pattern="^p\\ "    ,x=Value)] = TreatStd["nutrient"]
      Value[IsAuthor & grepl(pattern="^n\\+p"   ,x=Value)] = TreatStd["nutrient"]
      #--- Details
      IsAuthor = ( ( AuthorName %in% "Marina Scalon" )
                 & IsTreatExpo
                 & ( NameOrig %in% "exposition" )
                 )#end IsAuthor
      Value  [IsAuthor] = NA_character_
      Valid  [IsAuthor] = FALSE
      VName  [IsAuthor] = NA_character_
      #---~---


      #---~---
      #   Marina Semchenko. Treatment was different assemblages of plant compositions.
      # Assign to "minor treatment."
      #---~---
      IsAuthor = ( AuthorName %in% "Marina Semchenko" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "conspecific"     )] = TreatStd["minor"]
      Value[IsAuthor & ( Value %in% "heterospecific"  )] = TreatStd["minor"]
      #---~---



      #---~---
      #   Michael Scherer-Lorenzen. Treatment light
      #---~---
      IsAuthor         = ( AuthorName %in% "Michael Scherer-Lorenzen" ) & IsTreatLight
      Value [IsAuthor & (  Value %in% c(100) )] = TreatStd["control"]
      Value [IsAuthor & (! Value %in% c(100) )] = TreatStd["light"  ]
      #---~---



      #---~---
      #   Bill Shipley. Water treatments were multiple hydroponic solutions. Assign to
      # other. Most light experiment data were quantitative, keep only the full sunlight
      # information. Assign control/treatment based on CO2 level. 435 is assumed to
      # to be elevated because of the time of measurements. Set fertilisation experiments
      # as such.
      #---~---
      #--- Water
      IsAuthor = ( AuthorName %in% "Bill Shipley" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "ln" )] = TreatStd["other"]
      #--- Light
      IsAuthor         = ( AuthorName %in% "Bill Shipley" ) & IsTreatLight
      Value [IsAuthor & ( Value %in% c("full sunlight") )] = TreatStd["control"]
      Value [IsAuthor & ( Value %in% c("sunlight+lamp") )] = TreatStd["light"  ]
      IsTMI            =
         IsAuthor & ( ! Value %in% c("full sunlight", "sunlight+lamp") )
      Value [IsTMI] = NA_character_
      Valid [IsTMI] = FALSE
      VName [IsTMI] = NA_character_
      #--- CO2
      IsAuthor = ( AuthorName %in% "Bill Shipley" ) & IsTreatCO2
      Value[IsAuthor & ( Value %in% "400" )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "435" )] = TreatStd["co2"    ]
      #--- Nutrient
      IsAuthor = ( AuthorName %in% "Bill Shipley" ) & IsTreatPhosphorus
      Value[IsAuthor & (! is.na(NumValue) )] = TreatStd["nutrient"]
      #---~---


      #---~---
      #   Martijn Slot. Classes are straightforward.
      #---~---
      IsAuthor = ( AuthorName %in% "Martijn Slot" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "control" )] = TreatStd["control"    ]
      Value[IsAuthor & ( Value %in% "warming" )] = TreatStd["temperature"]
      #---~---


      #---~---
      #   Nick Smith.  Treatment is on heating and moisture change. Assume that 
      # experiments combining temperature and precipitation are "other".
      #---~---
      #--- Water. Skip it and use exposure instead.
      IsAuthor = ( ( AuthorName %in% "Nick Smith"        )
                 & IsTreatWater
                 & ( NameOrig   %in% "precipitation try" ) )
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #--- Exposition
      IsAuthor = ( ( AuthorName %in% "Nick Smith"        )
                 & IsTreatExpo
                 & ( NameOrig   %in% "exposition" ) )
      IsControl = Value %in% "natural environment; no warming; preccipitation ambient"
      IsWarming = Value %in% c("natural environment; high warming +4c; preccipitation ambient"
                              ,"natural environment; low warming +1.5c; preccipitation ambient"
                              ,"natural environment; medium warming +2.5c; preccipitation ambient"
                              )#end c
      IsPrecip  = Value %in% c("natural environment; no warming; preccipitation ambient -50%"
                              ,"natural environment; no warming; preccipitation ambient +50%"
                              )#end c
      IsOther   = Value %in% c("natural environment; high warming +4c; preccipitation ambient -50%"
                              ,"natural environment; high warming +4c; preccipitation ambient +50%"
                              ,"natural environment; low warming +1.5c; preccipitation ambient -50%"
                              ,"natural environment; low warming +1.5c; preccipitation ambient +50%"
                              ,"natural environment; medium warming +2.5c; preccipitation ambient -50%"
                              ,"natural environment; medium warming +2.5c; preccipitation ambient +50%"
                              )#end c
      Value[IsAuthor & IsControl] = TreatStd["control"    ]
      Value[IsAuthor & IsWarming] = TreatStd["temperature"]
      Value[IsAuthor & IsPrecip ] = TreatStd["water"      ]
      Value[IsAuthor & IsOther  ] = TreatStd["other"      ]
      #---~---



      #---~---
      #   Joao Paulo Souza. Matrix of water and CO2 enrichment.
      #---~---
      IsAuthor = ( AuthorName %in% "Joao Paulo Souza" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "380 ppm -without water stress" )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "380 ppm -water stress"         )] = TreatStd["water"  ]
      Value[IsAuthor & ( Value %in% "700 ppm -without water stress" )] = TreatStd["co2"    ]
      Value[IsAuthor & ( Value %in% "700 ppm -water stress"         )] = TreatStd["other"  ]
      #---~---



      #---~---
      #   Emily Swaine. Assign all to control based on the CO2 levels.
      #---~---
      IsAuthor = ( AuthorName %in% "Emily Swaine" ) & IsTreatCO2
      Value[IsAuthor & ( Value %in% c("360","400") )] = TreatStd["control"]
      #---~---


      #---~---
      #   Jose M. Torres-Ruiz.  Drought and irrigation experiment.
      #---~---
      IsAuthor = ( AuthorName %in% "Jose M. Torres-Ruiz" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "control"              )] = TreatStd["control"]
      Value[IsAuthor & ( Value %in% "localized irrigation" )] = TreatStd["water"  ]
      Value[IsAuthor & ( Value %in% "water stressed"       )] = TreatStd["water"  ]
      #---~---


      #---~---
      #   Helene Tribouillois.  Fertilisation and irrigation experiment.
      #---~---
      IsAuthor     = ( AuthorName %in% "Helene Tribouillois" ) & IsTreatExpo
      IsIrrigation = Value %in% "irrigation"
      IsOther      = Value %in% "irrigation and n fertilisation (100 kg/ha)"
      Value[IsAuthor & IsIrrigation] = TreatStd["water"]
      Value[IsAuthor & IsOther     ] = TreatStd["other"]
      #---~---


      #---~---
      #   Anthony Walker.  Variable treatment_other_levels is not the most informative
      # for this (other IDs may need to be loaded). Light treatment seems to refer to 
      # canopy position rather than light experiments. Save information to sun/shade flag.
      # If not applicable, save as control in treatment. Assign all to control based on 
      # the CO2 levels. For nutrient enhancement, they used a 0/1 code for the 
      # control/phosphorus treatment.
      #---~---
      #--- General
      IsAuthor        = ( AuthorName %in% "Anthony Walker" ) & IsTreatment
      Value[IsAuthor] = NA_character_
      Valid[IsAuthor] = FALSE
      VName[IsAuthor] = NA_character_
      #--- Light
      IsAuthor         = ( AuthorName %in% "Anthony Walker" ) & IsTreatLight
      IsShade   = 
         IsAuthor & ( grepl(pattern="^low"   ,x=Value) | ( Value %in% c("shade") ) )
      IsPartial = 
         IsAuthor & ( grepl(pattern="^middle",x=Value) )
      IsSunlit  = 
         IsAuthor & ( grepl(pattern="^high"  ,x=Value) | ( Value %in% c("sun","upper") ) )
      AncilID[IsShade | IsPartial | IsSunlit            ] = 443L
      Ancil  [IsShade                                   ] = "01 - Mostly Shaded"
      Ancil  [IsPartial                                 ] = "02 - Partially Shaded"
      Ancil  [IsSunlit                                  ] = "03 - Mostly Sun-Exposed"
      Value  [IsShade | IsPartial | IsSunlit            ] = NA_character_
      Valid  [IsShade | IsPartial | IsSunlit            ] = FALSE
      VName  [IsShade | IsPartial | IsSunlit            ] = NA_character_
      Value  [IsAuthor & ( Value %in% "not applicable" )] = TreatStd["control"]
      #--- CO2
      IsAuthor = ( AuthorName %in% "Anthony Walker" ) & IsTreatCO2
      Value[IsAuthor & (Value %in% "amb" )] = TreatStd["control"]
      Value[IsAuthor & (Value %in% "ele" )] = TreatStd["co2"    ]
      IsTMI        = IsAuthor & ( Value %in% "not applicable" )
      Value[IsTMI] = NA_character_
      Valid[IsTMI] = FALSE
      VName[IsTMI] = NA_character_
      #--- Phosphorus
      IsAuthor = ( AuthorName %in% "Anthony Walker" ) & IsTreatPhosphorus
      Value[IsAuthor & ( Value %in% c("not applicable","unfertilised") )] = 
                                                                       TreatStd["control" ]
      Value[IsAuthor & ( Value %in% c("fertilised","high","low")       )] =
                                                                       TreatStd["nutrient"]
      #--- Nitrogen
      IsAuthor = ( AuthorName %in% "Anthony Walker" ) & IsTreatNitrogen
      Value[IsAuthor & (  Value %in% c("not applicable","unfertilised") )] = 
                                                                       TreatStd["control" ]
      Value[IsAuthor & (! Value %in% c("not applicable","unfertilised") )] = 
                                                                       TreatStd["nutrient"]
      #---~---



      #---~---
      #   Christian Wirth. Information seemed to be related to the method used to infer
      # microenvironmental conditions. Discard information. Check treatment exposure for
      # details.
      #---~---
      #--- Light
      IsAuthor         = ( AuthorName %in% "Christian Wirth" ) & IsTreatLight
      Value [IsAuthor] = NA_character_
      Valid [IsAuthor] = FALSE
      VName [IsAuthor] = NA_character_
      #--- Details
      IsAuthor = ( ( AuthorName %in% "Christian Wirth"        )
                 & IsTreatExpo
                 & ( NameOrig   %in% "treatment" ) )
      IsOther  = Value %in% c("climate chamber","greenhouse","open top chambers"
                             ,"other","outside 'natural' vegetation","pots outside"
                             ,"shade houses")
      Value[IsAuthor & IsOther] = TreatStd["other"]
      #---~---



      #---~---
      #   Justin Wright.  We don't assign abandonment or land use histories to any
      # treatment, unless there is indication of fertilisation (but we assign to other
      # treatment as this is not the typical controlled experiment).
      #---~---
      IsAuthor  = ( AuthorName %in% "Justin Wright" ) & IsExperiment
      Value[IsAuthor & ( Value %in% "nitrogen"    )] = TreatStd["nutrient"]
      Value[IsAuthor & ( Value %in% "water table" )] = TreatStd["water"   ]
      
      #---~---



      #---~---
      #   Qiang Yu.  Treatment is on nutrients.
      #---~---
      #--- Treatment
      IsAuthor = ( AuthorName %in% "Qiang Yu" ) & IsTreatment
      Value[IsAuthor & ( Value %in% "c"                   )] = TreatStd["control" ]
      Value[IsAuthor & ( grepl(pattern="^n[0-9]",x=Value) )] = TreatStd["nutrient"]
      Value[IsAuthor & ( grepl(pattern="^p[0-9]",x=Value) )] = TreatStd["nutrient"]
      #--- Details
      IsAuthor = ( ( AuthorName %in% "Qiang Yu" )
                 & IsTreatExpo
                 & ( NameOrig %in% "exposition" )
                 )#end IsAuthor
      Value  [IsAuthor] = NA_character_
      Valid  [IsAuthor] = FALSE
      VName  [IsAuthor] = NA_character_
      #---~---



      #---~---
      #   Standardise values. If not in the list of expected categories, remove values
      # but do not change validity. These are values that may have been missed.
      #---~---
      IsValid          = Value %in% TreatStd
      Value[! IsValid] = NA_character_
      #---~---



      #---~---
      #   Standard names.
      #---~---
      OutAttribute = TreatStd
      names(OutAttribute) = NULL
      #---~---
    }#end if (AncilCoord)
   #---~---


   #---~---
   #   Return the standardised values. For some categorical variables, return the list of
   # standard values.
   #---~---
   Answer = tidyr::tibble( Value   = Value
                         , Valid   = Valid
                         , VName   = VName
                         , TraitID = TraitID
                         , Trait   = Trait
                         , AncilID = AncilID
                         , Ancil   = Ancil
                         )#end tidyr::tibble
   attr(Answer,"Levels") = OutAttribute
   return(Answer)
   #---~---
}#end function TRY_FixAncil_OrigValue_Str
#---~---




#---~---
#   This function seeks to convert data that is provided as ranges or inequalities.
#  These are less certain values so they are not ideal and must be flagged as such.
#---~---
TRY_Fix_Uncertain_Str <<- function(x,OffFactor=0.25){
   #---~---
   #   Set the answer to be the same as the input. This will likely cause failure for 
   # unaccounted cases.
   #---~---
   Answer    = tolower(x)
   NumAnswer = suppressWarnings(as.numeric(Answer))
   #---~---



   #---~---
   #   Replace labels such as initial "to" with range from 0".
   #---~---
   ToBegin       = ( grepl(pattern="^to"  ,x=Answer)
                   | grepl(pattern="^upto",x=Answer)
                   | grepl(pattern="^ti"  ,x=Answer)
                   | grepl(pattern="^tto" ,x=Answer)
                   )#end ToBegin
   Answer[ToBegin] = gsub(pattern="^to"  ,replacement="0-",x=Answer[ToBegin])
   Answer[ToBegin] = gsub(pattern="^upto",replacement="0-",x=Answer[ToBegin])
   Answer[ToBegin] = gsub(pattern="^ti"  ,replacement="0-",x=Answer[ToBegin])
   Answer[ToBegin] = gsub(pattern="^tto" ,replacement="0-",x=Answer[ToBegin])
   #---~---



   #---~---
   #   Replace labels such as "to" in the middle with -".
   #---~---
   MidTo         = ( grepl(pattern="[0-9]to[0-9]"   ,x=Answer)
                   | grepl(pattern="[0-9]to\\-[0-9]",x=Answer)
                   )#end MidTo
   Answer[MidTo] = gsub(pattern="to",replacement="-",x=Answer[MidTo])
   #---~---



   #---~---
   #   Replace labels such as "between X and Y" with "X-Y".
   #---~---
   Between         = (   grepl(pattern="^between"        ,x=Answer)
                     & ( grepl(pattern="[0-9]and\\-[0-9]",x=Answer)
                       | grepl(pattern="[0-9]and[0-9]"   ,x=Answer) )
                     )#end Between
   Answer[Between] = gsub(pattern="^between",replacement="" ,x=Answer[Between])
   Answer[Between] = gsub(pattern="and"     ,replacement="-",x=Answer[Between])
   #---~---



   #---~---
   #   Identify strings with "less" signs (i.e., <, <<, <=, less than).
   #---~---
   HasLTLE            = ( grepl(pattern="^<="       ,x=Answer)
                        | grepl(pattern="^<<"       ,x=Answer)
                        | grepl(pattern="^<[0-9]"   ,x=Answer)
                        | grepl(pattern="^<\\-[0-9]",x=Answer)
                        | grepl(pattern="^lessthan" ,x=Answer)
                        | grepl(pattern="^within"   ,x=Answer)
                        )#end HasLTLE
   Answer[HasLTLE]    = gsub(pattern="^<="      ,replacement="",x=Answer[HasLTLE])
   Answer[HasLTLE]    = gsub(pattern="^<<"      ,replacement="",x=Answer[HasLTLE])
   Answer[HasLTLE]    = gsub(pattern="^<"       ,replacement="",x=Answer[HasLTLE])
   Answer[HasLTLE]    = gsub(pattern="^lessthan",replacement="",x=Answer[HasLTLE])
   Answer[HasLTLE]    = gsub(pattern="^within"  ,replacement="",x=Answer[HasLTLE])
   MultFactor         = runif(n=sum(HasLTLE),min=1.-OffFactor,max=1.)
   NumAnswer[HasLTLE] = suppressWarnings(as.numeric(Answer[HasLTLE]))
   Answer[HasLTLE]    = as.character(signif(MultFactor * NumAnswer[HasLTLE],3))
   #---~---



   #---~---
   #   Identify strings with "greater" signs (i.e., >, >>, >=, greater than).
   #---~---
   HasGTGE            = ( grepl(pattern="^>="         ,x=Answer)
                        | grepl(pattern="^>>"         ,x=Answer)
                        | grepl(pattern="^>[0-9]"     ,x=Answer)
                        | grepl(pattern="^>\\-[0-9]"  ,x=Answer)
                        | grepl(pattern="^greaterthan",x=Answer)
                        | grepl(pattern="^over"       ,x=Answer)
                        )#end HasLTLE
   Answer[HasGTGE]    = gsub(pattern="^>="         ,replacement="",x=Answer[HasGTGE])
   Answer[HasGTGE]    = gsub(pattern="^>>"         ,replacement="",x=Answer[HasGTGE])
   Answer[HasGTGE]    = gsub(pattern="^>"          ,replacement="",x=Answer[HasGTGE])
   Answer[HasGTGE]    = gsub(pattern="^greaterthan",replacement="",x=Answer[HasGTGE])
   Answer[HasGTGE]    = gsub(pattern="^over"       ,replacement="",x=Answer[HasGTGE])
   MultFactor         = runif(n=sum(HasGTGE),min=1,max=1.+OffFactor)
   NumAnswer[HasGTGE] = suppressWarnings(as.numeric(Answer[HasGTGE]))
   Answer[HasGTGE]    = as.character(signif(MultFactor * NumAnswer[HasGTGE],3))
   #---~---



   #---~---
   #   Find intermediate value for range strings.
   #---~---
   HasRange         = ( grepl( pattern="[0-9]\\-[0-9]"   ,x=Answer)
                      | grepl( pattern="[0-9]\\-\\-[0-9]",x=Answer) )
   if (any(HasRange)){
      Answer[HasRange] = mapply( FUN      = TRY_Fix_MeanRange_Scalar
                               , x        = as.list(Answer[HasRange])
                               , SIMPLIFY = TRUE
                               )#end mapply
   }#end if (any(HasRange))
   #---~---

   return(Answer)
}#end TRY_Fix_Uncertain_Str
#---~---




#---~---
#   This function transforms ranges into a string that can be converted to character.
#---~---
TRY_Fix_MeanRange_Scalar <<- function(x){
   #---~---
   #   To preserve negative values, we temporarily replace - denoting negative numbers
   # with another character (#).
   #---~---
   Answer = x
   Answer = gsub(pattern="^\\-"  ,replacement="#",x=Answer)
   Answer = gsub(pattern="\\-\\-",replacement="#",x=Answer)
   Answer = unlist(strsplit(x=Answer,split="-"))
   Answer = gsub(pattern="#"     ,replacement="-",x=Answer)
   #---~---


   #--- Replace string with the average between the first two entries.
   AnsLwr = suppressWarnings(as.numeric(Answer[1]))
   AnsUpr = suppressWarnings(as.numeric(Answer[2]))
   Answer = as.character(runif(n=length(x),min=AnsLwr,max=AnsUpr))
   #---~---

   return(Answer)
}#end TRY_Fix_MeanRange_Scalar
#---~---




#---~---
#   This function ensures dates have all the same length (adding zeroes to days and months).
#---~---
Fix_TRY_DateZero <<- function(x){
   #---~---
   #   Return nothing in case x is an empty vector.
   #---~---
   if (length(x) %eq% 0){
      #--- Return input.
      Answer = x
      #---~---
   }else{
      #---~---
      #   We use slashes as separators. Sometimes the dates may be separated by dashes, so we
      # replace them
      #---~---
      Answer = x
      Answer = do.call("rbind",mapply(strsplit,Answer,MoreArgs=list(split="-")))
      #---~---


      #--- Append zeroes to entries that need one.
      AddZero         = nchar(Answer) == 1
      Answer[AddZero] = paste0("0",Answer[AddZero] )
      if (! is.matrix(Answer)) browser()
      Answer          = apply(X=Answer,MARGIN=1,FUN=paste,collapse="-")
      names(Answer)   = NULL
      #---~---
   }#end if (length(x) %eq% 0)
   #---~---

   return(Answer)
}#end Fix_TRY_DateZero
#---~---
