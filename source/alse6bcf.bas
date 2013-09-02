   KEY OFF
   DEFINT I:DEFDBL C
   EXT$=".RTE"

   REM Set to 1 to Debug the database lookup routine
   DBG = 0

   REM Declare "fast" retrieval key arrays and Open the databases
   IMAXFILES=3:FFILE%=2
   DIM NREC(IMAXFILES+1),FN$(IMAXFILES+2)
   DIM KEYSIZE(IMAXFILES+1),RECSIZE(IMAXFILES+2)

   REM ALLOCATE OUR OWN KEY STORAGE AREA WITHIN INDIVIDUAL 64K SEGMENTS
   MAXKYOFF#=65535.
   IMAXKEYS=(MAXKYOFF#/4.)-1.
   DIM KYA2#(IMAXKEYS)
   DIM KYA3#(IMAXKEYS)
   DIM KYA4#(IMAXKEYS)

   REM Database Query variables
   RFILES%=3         :REM Files to query (Default is Airports)
   IMAXQUERY = 1001  :REM Maximum # of returned result matches
   IMAXRESULTS=6     :REM Maximum # of returned fields (Columns)
   DIM QRESULT#(IMAXQUERY,IMAXRESULTS)

   REM Results SORT field
   DIM SF$(IMAXRESULTS),SF%(IMAXRESULTS)
   SF$(0)="ID":SF%(0)=0

   REM Things that can be returned by setting a non zero column position
   REM  Note, column position may be changed by the calling routine
   REM  If the value is zero, the query calculation is NOT to be run
   REM The ID of the point is always returned in column 0
   REM   QF         FILE% of data base matched
   REM   QLEG       Distance "TO" the point
   REM   QTDS       Total distance
   REM   QDELTA     Delta difference in distance
   REM   QFUEL      Fuel Price
   REM   QCOST      Extended cost
   REM   QROUTE     Suggested optimal ordering of waypoints

   REM   QSTATE     Not returned in the array, but causes STATE to be displayed
   REM   QCITY      Not returned in the array, but causes CITY to be displayed

   REM Pack a 1 to 5 character key into a 32 bit integer
   DEF FNPKEY#( KY$ )

      LKY#=0
      FOR IK = 1 TO LEN(KY$)
        LKY# = (64*LKY#) + ASC(MID$(KY$,IK,1))-32
      NEXT IK

      FNPKEY# = LKY#
   END DEF

   REM Expand the key string from a packed key value
   DEF FNEKEY$( KY# )

      LKY# = KY#
      FKY$=""
      WHILE LKY# > 0
         C = (LKY# AND 63)
         LKY# = (LKY# - C) / 64
         FKY$ = CHR$(32 + C) + FKY$
      WEND

      FNEKEY$ = FKY$

   END DEF

   REM ***** Comment out this code to save program memory
   REM REM Pack string key into selected key array element
   REM FUNCTION SKEY#(A#(),REC%, KY$) STATIC

      REM A#(REC%) = FNPKEY#(KY$)
      REM SKEY# = A#(REC%)

   REM END FUNCTION

   REM REM Expand the key string selected from a key array
      REM FUNCTION GKEY$(A#(),REC%) STATIC

      REM   GKEY$ = FNEKEY$(A#(REC%))

      REM END FUNCTION

   REM GOTO 50
   REM FOR I = FFILE% TO IMAXFILES+1
   REM FOR II = 0 TO 5
   REM    KY$="F"+CHR$(48+I)+CHR$(II+65)
   REM    I# = FNPKEY#(KY$)
   REM    IF I = 2 THEN KYA2#(II) = I#
   REM    IF I = 3 THEN KYA3#(II) = I#
   REM    IF I = 4 THEN KYA4#(II) = I#
   REM    PRINT "Setting ";i;ii,ky$;i#
   REM NEXT II
   REM NEXT I

   REM II = 1
   REM WHILE II <> 0
   REM     INPUT "Test value of what key pair (file,record)";II,i

   REM     IF II = 2 THEN KY$ = FNEKEY$(KYA2#(I))
   REM     IF II = 3 THEN KY$ = FNEKEY$(KYA3#(I))
   REM     IF II = 4 THEN KY$ = FNEKEY$(KYA4#(I))
   REM     IF II = 2 THEN KYG$ = GKEY$(KYA2#(),I)
   REM     IF II = 3 THEN KYG$ = GKEY$(KYA3#(),I)
   REM     IF II = 4 THEN KYG$ = GKEY$(KYA4#(),I)
   REM     PRINT "Value for file";II;"record";i;"is ";ky$,KYG$
   REM WEND
   REM INPUT "Press enter to continue";O$
REM 50 REM

   GOSUB CLRKEYS
   GOSUB 1000

   PI=3.1415926536#:DG = 180/PI: REM  PI and Degree conversion constants
   FAACYCLE$=""

   REM Printer defaults
   DFPRTR$="LPT1:"
   PRINIT$=CHR$(15)+CHR$(27)+CHR$(48)+CHR$(13)
   PRSUPER$=CHR$(27)+"S0"
   SUB$=CHR$(27)+"S1"
   NORMAL$=CHR$(27)+"T"

   REM Degree symbol for the Screen and Printer
   DIM DEGSYM$(2)
   DEGSYM$(0)="ø"  :REM Screen
   DEGSYM$(1)="ø"  :REM Printer

   REM Defaults for airplane performance
   CLIMBSP=.75:FPM=500:CALT=9000:KTS=158
   FUELLOAD=90      :REM Departure fuel load (Gal)
   FUELCAP=90       :REM Max fuel capacity
   FUELMIN=13       :REM Minimum confortable fuel
   FUELBURN=13.3    :REM Cruise fuel burn rate  (Gal/hr)
   HOURLY= 10       :REM Allowance for consumable hourly expenses
   RUNWYHD=170      :REM No wind runway heading

       REM Allowable increase in direct routing distance
   QINCREASE = 1.10   :REM 10% increase
   QROUTEWIDTH = 60   :REM OR within nm, which ever is greater

   REM configuration defaults
   CONFIG$="ALSE6B.CFG"
   PROFILE$="ALSE6B.DEF"

   REM read the printer configuration file
   ON ERROR GOTO 65
   OPEN "I",#1,CONFIG$

   INPUT #1,I$:IF I$<> "" THEN PROFILE$=I$

   INPUT #1,I$:IF I$<> "" THEN DFPRTR$=I$

   INPUT #1,II:PRINIT$=""
   FOR I=1 TO II:INPUT#1,III:PRINIT$=PRINIT$+CHR$(III):NEXT I

   INPUT #1,II:DEGSYM$(1) = ""
   FOR I=1 TO II:INPUT#1,III:DEGSYM$(1)=DEGSYM$(1)+CHR$(III):NEXT I

   INPUT #1,II:PRSUPER$ = ""
   FOR I=1 TO II:INPUT#1,III:PRSUPER$=PRSUPER$+CHR$(III):NEXT I

   INPUT #1,II:SUB$ = ""
   FOR I=1 TO II:INPUT#1,III:SUB$=SUB$+CHR$(III):NEXT I

   INPUT #1,II:NORMAL$ = ""
   FOR I=1 TO II:INPUT#1,III:NORMAL$=NORMAL$+CHR$(III):NEXT I

   CLOSE #1
   FONE%=0
   GOTO 69
65 RESUME 69
69 ON ERROR GOTO 0

   REM get user defaults for airplane performance
   GOSUB 80
   IF STATUS$="OK" THEN GOSUB 85:CLOSE #1
   GOTO 90

80 REM Open the performance profile
   ON ERROR GOTO 82
   STATUS$="OK"
   OPEN "I",#1,PROFILE$
   GOTO 84
82 STATUS$="NO FILE"
   RESUME 84
84 ON ERROR GOTO 0
   RETURN

85 REM Read base aircraft performance specification
   INPUT #1,KTS,CLIMBSP,FPM,CALT
   INPUT #1,QINCREASE,QROUTEWIDTH
   INPUT #1,FUELLOAD,FUELCAP,FUELMIN,FUELBURN,HOURLY
   INPUT #1,RUNWYHD
   RETURN

90 AVGGS=KTS
   WAYPT=0
   MAXPTS = 40
   DIM PL$(MAXPTS),IDENT$(MAXPTS),FREQ$(MAXPTS),MV(MAXPTS)
   DIM TC(MAXPTS),DS(MAXPTS),ELEV(MAXPTS)
   DIM LADEG(MAXPTS),LODEG(MAXPTS),LAMIN(MAXPTS),LOMIN(MAXPTS)
   DIM TYPE$(MAXPTS),CTY$(MAXPTS),ST$(MAXPTS)
   DIM AIRSP(MAXPTS),WS(MAXPTS),WD(MAXPTS)

   REM Used during fuel cost swags
   DIM FUELP(MAXPTS),FUELARRIVE(MAXPTS),FUELADD(MAXPTS),FUELUSEABLE(MAXPTS)

   REM Used for Route queries
   DIM QRTEWPT%(MAXPTS)
   QENDPT%=0: REM Route Begin/End point
   QSFTPT%=1: REM Suggested intermediate point, not required to be flown
   QHRDPT%=2: REM Required intermediate waypoint, must be flown

   DIM QRECT!(1,1)

   GOTO 40000:  Rem Jump to Main Menu

CLRKEYS: PRINT"clearing retrieval keys..."
         FOR III=1 TO IMAXFILES
           FOR II=0 TO IMAXKEYS-1
             IF I = 2 THEN KYA2#(II) = 0
             IF I = 3 THEN KYA3#(II) = 0
             IF I = 4 THEN KYA4#(II) = 0
           NEXT II
         NEXT III
         RETURN

SORT: REM First, read the file to get the keys
      IF NREC(FILE%) < 2 THEN RETURN
      PRINT "Indexing";NREC(FILE%);"records... "
      FIELD #FILE%,KEYSIZE(FILE%) AS ID$, RECSIZE(FILE%)-KEYSIZE(FILE%) AS DATA$
      RESORT = 0

      REM  We need to allocate another array, so free up memory not needed.
      REM  This is okay as we will re-initial everything when completed.
      IF FILE% <> 2 THEN ERASE KYA2#
      IF FILE% <> 3 THEN ERASE KYA3#
      IF FILE% <> 4 THEN ERASE KYA4#
      DIM NEWPOS#(IMAXKEYS)

      FOR I = 1 TO NREC(FILE%)
         GET #FILE%,I
         NEWPOS#(I) = I
         IF FILE% = 2 THEN KYA2#(I) = FNPKEY#(ID$)
         IF FILE% = 3 THEN KYA3#(I) = FNPKEY#(ID$)
         IF FILE% = 4 THEN KYA4#(I) = FNPKEY#(ID$)
      NEXT I

      REM Now sort ascending by taking each key and finding the smallest
      REM   key in the file from that point foward and swapping it.
      FIELD #FILE%,RECSIZE(FILE%) AS DATA$
      J = NREC(FILE%) - 1
      WHILE J >= 1
         IF INT(J/750)*750 = J THEN PRINT CHR$(13);"Records left ";J;"/";NREC(FILE%);

         IF FILE% = 2 THEN PKY# = KYA2#(J+1):KY# = KYA2#(J)
         IF FILE% = 3 THEN PKY# = KYA3#(J+1):KY# = KYA3#(J)
         IF FILE% = 4 THEN PKY# = KYA4#(J+1):KY# = KYA4#(J)

         REM If Key(J+1) < Key(J), we are out of ascending sort order
         IF PKY# < KY# THEN
            RESORT = 1

            REM Search forward to find the spot for this key
            II = NREC(FILE%)
            I = J + 1
            WHILE I <= NREC(FILE%)

               IF FILE% = 2 THEN PKY# = KYA2#(I)
               IF FILE% = 3 THEN PKY# = KYA3#(I)
               IF FILE% = 4 THEN PKY# = KYA4#(I)

               REM As the bottom end of the list is already sorted in
               REM ascending, we can stop searching as soon as we've
               REM gone past the correct slot
               IF KY# < PKY# THEN II = I-1:I = NREC(FILE%)

               I = I + 1
            WEND

            REM Save the record at J
            RECNUM = NEWPOS#(J)

            REM Ripple move all Records from J+1 to I toward front of the file
            FOR I = J+1 TO II
               NEWPOS#(I-1) = NEWPOS#(I)

               IF FILE% = 2 THEN KYA2#(I-1) = KYA2#(I)
               IF FILE% = 3 THEN KYA3#(I-1) = KYA3#(I)
               IF FILE% = 4 THEN KYA4#(I-1) = KYA4#(I)

            NEXT I

            REM Write the saved record (J) into correct position (II)
            NEWPOS#(II) = RECNUM
            IF FILE% = 2 THEN KYA2#(II) = KY#
            IF FILE% = 3 THEN KYA3#(II) = KY#
            IF FILE% = 4 THEN KYA4#(II) = KY#

         ENDIF
         J = J -1

      WEND

      REM If the file order has been changed
      IF RESORT > 0 THEN

         REM We will copy the input to minimize the amount record by record
         REM  copying we will have to do.  This assumes that a bunch of the
         REM  file has not changed, so we will get some benefit to the copy.

         PRINT:PRINT "Creating temporary work file"
         CLOSE #FILE%
         SHELL "COPY "+FN$(FILE%)+DEXT$+" "+FN$(FILE%)+".$TP"

         REM Use a temporary file to reorder the records
         OPEN "R",#1,FN$(FILE%)+".$TP",RECSIZE(FILE%)
         FIELD #1,RECSIZE(FILE%) AS RECORD$
         IF LOF(1)/RECSIZE(FILE%) <> NREC(FILE%) THEN
            CLOSE
            PRINT
            PRINT "Internal error with temporary file: ";FN$(FILE%)+".$TP"
            PRINT "Check the amount of available disk space."
            PRINT FN$(FILE%)+DEXT$+" is not indexed properly."
            PRINT "Correct what ever is causing the above error and then Work"
            PRINT "with that Data type to rename an ID, thus causing a  reindexing"
            PRINT "of the data to take place."
            PRINT "Press ENTER to acknowledge...";I$
            RETURN
         ENDIF

         OPEN "R",#FILE%,FN$(FILE%)+DEXT$,RECSIZE(FILE%)
         FIELD #FILE%,RECSIZE(FILE%) AS DATA$

         REM Read sorted keys and fetch the original record in sorted order
         FOR I = 1 TO NREC(FILE%)

             RECNUM = NEWPOS#(I)
             IF RECNUM <> I THEN

                GET #FILE%,RECNUM
                LSET RECORD$=DATA$
                PUT #1,I
             ENDIF
             IF INT(I/350)*350 = I THEN PRINT CHR$(13);"Phase 2 ordering ";I;"/";NREC(FILE%);
         NEXT I

         REM Now copy the sorted results back from the Temporary file
         PRINT:PRINT "Renaming temporary file to permanent file"
         CLOSE
         KILL FN$(FILE%)+DEXT$
         SHELL "RENAME "+FN$(FILE%)+".$TP "+FN$(FILE%)+DEXT$

      ENDIF

      ERASE NEWPOS#
      RETURN

FMTCOORD:
      I$=STR$(ASC(LADEG$)-90)+"."+STR$(ASC(LAMIN$))+"."+STR$(ASC(LASEC$))
      GOSUB PARSELAT
      I$=STR$(ABS(ILODEG-180))+"."+STR$(ILOMIN)+"."+STR$(ILOSEC)
      IF ILODEG<180 THEN I$="E"+I$
      GOSUB PARSELONG
      RETURN

PARSECOORD:
      REM Parse the input Latitude format into our normal short hand notation
      I$=LAT$
      GOSUB PARSELAT

      REM Parse the input Longitude format into our normal short hand notation
      I$=LONG$
      GOSUB PARSELONG
      RETURN

INPUTLAT: PRINT"Enter 'degrees-minutes-seconds'"
          INPUT"Latitude (DD-MM-DM)";I$
          IF I$ = "" THEN RETURN

PARSELAT: REM Parse and Check Latitude
          III=0:II=1:LAT$="":MJ=90
          II$=I$+" "
PLAT1:    IF LEFT$(II$,1) = " " THEN II$=MID$(II$,2):GOTO PLAT1
          IF LEFT$(II$,1) = "N" OR LEFT$(II$,1) = "n" THEN II=2
          IF LEFT$(II$,1) = "S" OR LEFT$(II$,1) = "s" THEN II=2
          FOR I=II TO LEN(II$)
             IF MID$(II$,I,1) <= "9" AND MID$(II$,I,1) >= "0" OR I = II THEN GOTO PLAT2
             J=ABS(VAL(MID$(II$,II,I-II)))
             IF J > MJ THEN III=1000
             LAT$=LAT$+RIGHT$(STR$(100+J),2)+"-"
             III=III+1:II=I+1:MJ=59
PLAT2:    NEXT I
          LAT$=LEFT$(LAT$,LEN(LAT$)-1)
          IF LEN(LAT$) <> 8 OR III <> 3 THEN PRINT"Entry syntax error for ";I$:GOTO INPUTLAT
          IF LEFT$(II$,1) = "S" OR LEFT$(II$,1) = "s" OR LEFT$(II$,1) = "-" THEN LAT$="S"+LAT$ ELSE LAT$="N"+LAT$
          RETURN

INPUTLONG: PRINT"Enter 'degrees-minutes-seconds'"
           INPUT"Longitude (DD-MM-DM)";I$
           IF I$ = "" THEN RETURN

PARSELONG: REM Parse and Check Longitude
           III=0:II=1:LONG$="":MJ=180
           II$=I$+" "
PLONG1:    IF LEFT$(II$,1) = " " THEN II$=MID$(II$,2):GOTO PLONG1
           IF LEFT$(II$,1) = " " THEN II$=MID$(II$,2)
           IF LEFT$(II$,1) = "W" OR LEFT$(II$,1) = "w" THEN II=2
           IF LEFT$(II$,1) = "E" OR LEFT$(II$,1) = "e" THEN II=2:II$="-"+MID$(II$,2)
           FOR I=II TO LEN(II$)
              IF MID$(II$,I,1) <= "9"  AND MID$(II$,I,1) >= "0" OR I = II THEN GOTO PLONG2
              J=VAL(MID$(II$,II,I-II))
              IF J > MJ THEN III=1000
              IF III=0 THEN LONG$=RIGHT$("   "+STR$(J),3) ELSE LONG$=LONG$+RIGHT$(STR$(100+J),2)
              LONG$=LONG$+"-"
              III=III+1:II=I+1:MJ=59
PLONG2:   NEXT I
          LONG$=LEFT$(LONG$,LEN(LONG$)-1)
          IF LEN(LONG$) <> 9 OR III <> 3 THEN PRINT"Entry syntax error for ";i$:GOTO INPUTLONG
          IF LEFT$(II$,1) = "-" THEN LONG$="E"+LONG$ ELSE LONG$="W"+LONG$
          IF MID$(LONG$,2,1) = " " THEN LONG$=" "+LEFT$(LONG$,1)+MID$(LONG$,3)
          RETURN

SETLATLONG: REM Set Latitude and Longitude into a database record
            IF LEFT$(LAT$,1) = "S" OR LEFT$(LAT$,1)="s" THEN LAT$="-"+MID$(LAT$,2)
            IF LEFT$(LAT$,1) = "N" OR LEFT$(LAT$,1)="n" THEN LAT$=" "+MID$(LAT$,2)
            LSET LADEG$=CHR$(VAL(MID$(LAT$,1,3))+90)
            LSET LAMIN$=CHR$(VAL(MID$(LAT$,5,2)))
            LSET LASEC$=CHR$(VAL(MID$(LAT$,8,2)))

            IF LEFT$(LONG$,1) = " " THEN LONG$=MID$(LONG$,2,1) + " " + MID$(LONG$,3)
            IF LEFT$(LONG$,1) = "E" OR LEFT$(LONG$,1)="e" THEN LONG$="-"+MID$(LONG$,2)
            IF LEFT$(LONG$,1) = "W" OR LEFT$(LONG$,1)="w" THEN LONG$=" "+MID$(LONG$,2)
            ILODEG=VAL(MID$(LONG$,1,4))+180
            ILOMIN=VAL(MID$(LONG$,6,2))
            ILOSEC=VAL(MID$(LONG$,9,2))
            LSET LODEG$=CHR$(ILODEG - 256*INT(ILODEG/256))
            LSET LOMIN$=CHR$(ILOMIN)
            LSET LOSEC$=CHR$( (ILOSEC*2) + INT(ILODEG/256))
            RETURN

UNPKFREQ: REM Unpack the frequency as stored in the file
          REM Note, this intentionally falls into FMTFREQ
      J!=CVL(FREQ$)/1000!+125!

FMTFREQ: REM Format a frequency for standardize aviation output
      REM   Input is J!, Output is I$
      IF J! < 10! THEN I$=STR$(J!*100): RETURN
      J!=INT((J!+.0005)*1000)
      IF J! = 0 then I$="":RETURN
      IF J! > 150*1000! THEN I$=STR$(J!/100000): RETURN
      I$=STR$(J!)
      I$=LEFT$(I$,LEN(I$)-3)+"."+RIGHT$(I$,3)
      I$=MID$(I$,2)
      IF LEFT$(I$,1)="0" THEN I$=LEFT$(I$,LEN(I$)-1)+" "
      IF RIGHT$(I$,1)="0" THEN I$=LEFT$(I$,LEN(I$)-1)+" "
      RETURN

INPUTMAGV: REM Accept a new Magnetic Variation into VAR$
           INPUT"Magnetic variation";I$
           IF I$="" THEN RETURN
PARSEMAGV: IF LEFT$(I$,1)="E" OR LEFT$(I$,1)="e" OR LEFT$(I$,1)="W" OR LEFT$(I$,2)="w" THEN I = 2 ELSE I = 1
           IF LEFT$(I$,1)="?" THEN VAR$="?":RETURN
           I=VAL(MID$(I$,I))
           IF I <= -128 THEN VAR$="?":RETURN
           IF LEFT$(I$,1)="E" OR RIGHT$(I$,1)="E" OR LEFT$(I$,1)="e" OR RIGHT$(I$,1)="e" THEN I =-1*I
           VAR$=STR$(ABS(I))
           IF I < 0 THEN VAR$=VAR$+"E"
           IF I > 0 THEN VAR$=VAR$+"W"
           RETURN

SETMAGV: REM Set Magnetic Variation into a data record
         I$=VAR$
         IF LEFT$(I$,1)="?" OR VAR$ = "" THEN LSET DEV$=CHR$(0):RETURN
         IF LEFT$(I$,1)="E" OR LEFT$(I$,1)="e" OR LEFT$(I$,1)="W" OR LEFT$(I$,2)="w" THEN II = 2 ELSE II = 1
         II=VAL(MID$(I$,II))
         IF LEFT$(I$,1)="E" OR RIGHT$(I$,1)="E" OR LEFT$(I$,1)="e" OR RIGHT$(I$,1)="e" THEN II =-1*II
         LSET DEV$=CHR$(II+128)
         RETURN

            REM Expand Revision date
FMTREVDATE: II$ = MID$(STR$(CVL(I$)),2)
            II$ = MID$(II$,5,2)+"/"+MID$(II$,7,2)+"/"+LEFT$(II$,4)
            RETURN

             REM Compact Revision date mm/dd/yyyy into Sortable long binary
PACKREVDATE: I = INSTR(1,I$,"/")
             II = INSTR(I+1,I$,"/")
             J& = VAL(MID$(I$+"    ",II+1,4))              :REM Year
             IF J& < 100 THEN J&=1900+J&
             J& = (J& * 100!) + VAL(LEFT$(I$,I-1))         :REM Month
             J& = (J& * 100!) + VAL(MID$(I$,I+1,II-I-1))   :REM Day
             II$ = MKL$(J&)
             RETURN

PUTNAV: REM Put a Navaid record into the database at RECNUM
        GOSUB SETLATLONG
        LSET CITY$=CTY$
        LSET STATE$=ST$
        LSET ELEV$=MKI$(VAL(ELE$))
        LSET APT$=NAM$
        LSET FREQ$=MKL$((VAL(TWR$)-125)*1000)
        GOSUB SETMAGV:  REM Set Magnetic Variation into record buffer

        I$=CYCLE$:GOSUB PACKREVDATE:LSET REVDATE$ = II$

        PUT #FILE%,RECNUM
        RETURN

PUTFIX: REM Put a Fix record into the database at RECNUM
        GOSUB SETLATLONG
        LSET STATE$=ST$
        LSET APT$=NAM$
        GOSUB SETMAGV:  REM Set Magnetic Variation into record buffer
        LSET REFID$=REF1$
        LSET RADIAL$=MKI$(RADIAL)
        LSET DIST$=MKI$(10*DIST)

        I$=CYCLE$:GOSUB PACKREVDATE:LSET REVDATE$ = II$

        PUT #FILE%,RECNUM
        RETURN

PUTAPT: REM Put an Airport record into the database at RECNUM
        GOSUB SETLATLONG
        LSET CITY$=CTY$
        LSET STATE$=ST$
        LSET ELEV$=MKI$(VAL(ELE$))
        LSET APT$=NAM$
        LSET FREQ$=MKL$((VAL(TWR$)-125)*1000)
        GOSUB SETMAGV:  REM Set Magnetic Variation into record buffer
        LSET FUEL$=MKI$(VAL(FBO$)*1000)

        I$=CYCLE$:GOSUB PACKREVDATE:LSET REVDATE$ = II$

        IF DTLCHANGE$ = "TRUE" THEN GOSUB PUTAPTDTL

        PUT #FILE%,RECNUM
        RETURN

PUTAPTDTL: REM Update AIRPORT Details (Non indexed file)
           IF ATIS$="" AND RNWY$ = "" AND FBONAME$ = "" AND TFREQS$ = "" AND GROUND$="" AND APPROACH$="" THEN RETURN
           AINDX& = CVL(APTINFO$)

           IF AINDX& > 0 THEN ADDING=0:GOTO APTDTL2

           REM Adding new information at the end of the file
           ADDING = 1
           IF LOF(APTDTL%) > 0 THEN GOTO APTDTL1
           II$="xxxxxxxx"+CHR$(13)+CHR$(10)
           II$=II$+"Airport Details"+CHR$(13)+CHR$(10)
           II$=II$+" ID,Frequencies,ATIS,Ground,Approach,Runways, FBO"+CHR$(13)+CHR$(10)
           AINDX& = LEN(II$)
           II$=LEFT$(STR$(AINDX&)+"        ",8)+MID$(II$,9)
           LSET DB$=II$
           PUT #APTDTL%,1

           REM Update the APTINFO$ variable with where we are saving these details
APTDTL1:   GET #APTDTL%,1
           AINDX& = VAL(DB$)
           LSET APTINFO$=MKL$(AINDX&)

APTDTL2:   III$=CHR$(34)+","+CHR$(34)
           WHILE RIGHT$(FBONAME$,1) = " "
              FBONAME$=LEFT$(FBONAME$,LEN(FBONAME$)-1)
           WEND
           II$=CHR$(34)+ID$+III$+TFREQS$+III$+ATIS$+III$+GROUND$+III$+APPROACH$+III$
           II$=II$+RNWY$+III$+FBONAME$+III$+CHR$(34)

           REM Get the initial buffer that will start the record
           I = INT(AINDX&/RECSIZE(APTDTL%)):II = AINDX& - (I * RECSIZE(APTDTL%)) + 1
           GET #APTDTL%,I+1
           III$=DB$

           REM Is this an ADD of new data, or update of existing details
           IF ADDING = 1 THEN GOTO APTDTL3

           REM Get all the old record
           J = 1
           WHILE INSTR(II,III$,CHR$(13)) = 0
              J = J + 1
              GET #APTDTL%,I+J
              III$=III$+DB$
           WEND

           REM compute where the old data resides and If we can update inplace
           J = INSTR(II,III$,CHR$(13))
           IF LEN(II$) > (J - II) THEN ADDING = 1:GOTO APTDTL3

           REM Pad out to the size of the existing hole
           WHILE LEN(II$) < (J - II)
              II$ = LEFT$(II$,LEN(II$)-1) + "*" + RIGHT$(II$,1)
           WEND
           GOTO APTDTL4

           REM Adding to the end, must update our free space pointer in 1st record
APTDTL3:   II$=II$ + CHR$(13)+CHR$(10)
           GET #APTDTL%,1
           AINDX&=VAL(DB$)
           LSET APTINFO$=MKL$(AINDX&)
           LSET DB$=LEFT$(STR$(AINDX&+LEN(II$))+"        ",8)+MID$(DB$,9)
           PUT #APTDTL%,1
           IF I = 0 THEN III$=DB$+MID$(III$,RECSIZE(APTDTL%)+1):GOTO APTDTL4

           REM Get the buffer that will start this new record
           I = INT(AINDX&/RECSIZE(APTDTL%)):II = AINDX& - (I * RECSIZE(APTDTL%)) + 1
           GET #APTDTL%,I+1
           III$=DB$


APTDTL4:   REM
           IF II > 1 THEN III$=LEFT$(III$,II-1)+II$+MID$(III$,II+LEN(II$)) ELSE III$ = II$+MID$(III$,LEN(II$)+1)
           LSET DB$=LEFT$(III$,RECSIZE(APTDTL%))
           PUT #APTDTL%,I+1

           II = 1
           WHILE LEN(III$) > (II*RECSIZE(APTDTL%))
              LSET DB$=MID$(III$,(II*RECSIZE(APTDTL%))+1)
              II = II + 1
              PUT #APTDTL%,I+II
           WEND

           RETURN

SORTNAV: REM Sort the Navaids
         FILE%=2
         GOTO SORTANY

SORTAPT: REM Sort the Airports
         FILE%=3
         GOTO SORTANY

SORTFIX: REM Sort the Fixes
         FILE%=4
         REM Fall into the SORTANY subroutine

         REM Close all the files, re-open, and force getting new keys
SORTANY: GOSUB 1000
         GOSUB SORT
         CLOSE

         REM Now cause everything to be re-initialized
         RUN

100 REM Search all the databases
    IF INSTR(1,I$," ") > 0 THEN I$=LEFT$(I$,INSTR(1,I$," ")-1)
    III$=I$
    FILE%=4  :  REM         Search FIXES database
    IF LEN(I$)<> KEYSIZE(FILE%) THEN 120

    GOSUB 400
    IF II$<>ID$ THEN I$ = III$:GOTO 120

    REM We found the fix, but we would like to have the VOR reference frequency
    I$=REFID$:FILE%=2:GOSUB 400:FUZZY$=FREQ$(WAYPT)

    I$=III$:FILE%=4:GOSUB 400:FREQ$(WAYPT)=FUZZY$

    PL$(WAYPT)=REFID$+" @r"+MID$(STR$(CVI(RADIAL$)),2)+"/"+MID$(STR$(CVI(DIST$)/10),2)+"nm"
    CTY$(WAYPT)=PL$(WAYPT)
    RETURN

120 FILE%=2  :  REM         Search VOR/NDB database
    IF LEN(I$) > KEYSIZE(FILE%) THEN 130

    GOSUB 400
    IF II$=ID$ THEN PL$(WAYPT)=CITY$:RETURN
    I$ = III$

130 REM         Prepare to search the AIRPORT database
    FILE%=3
    IF LEN(I$) > KEYSIZE(FILE%) THEN 170
    IF LEN(I$) > 3 THEN GOSUB 190
    REM         SEARCH AIRPORT DATABASE
    GOSUB 400

    IF II$=ID$ THEN PL$(WAYPT)=APT$:IDENT$(WAYPT)=III$

170 REM         All appropriate databases have been searched
    RETURN

190 REM Handle stripping of special airport prefix
    IF INSTR(1,"KPC",LEFT$(I$,1))=0 THEN RETURN
    FOR I=1 TO 4
        IF MID$(I$,I,1) < "A" THEN RETURN
    NEXT I
    I$=MID$(I$,2)
    RETURN

    REM SEARCH ANY DATABASE FILE FOR RECORD IDENTIFIED BY I$
    REM  Note, I$ is destroyed.  A match will return ID$=II$
400 I=1:II=NREC(FILE%)
    IF DBG > 0 THEN PRINT "Looking up ";I$,"There are ";II;" records in file";FILE%
    ON FILE%-1 GOSUB 1120,1130,1140
    II$=LEFT$(I$+"     ",KEYSIZE(FILE%))
    IF NREC(FILE%) = 0 THEN LSET ID$=MID$("~~~~~~~",1,KEYSIZE(FILE%)):GOTO 490
    PKY# = FNPKEY#(II$):IF DBG > 0 THEN PRINT "   Packed search key is";PKY#
430 III=INT((I+II)/2)
    IF FILE%=2 THEN KY# = KYA2#(III)
    IF FILE%=3 THEN KY# = KYA3#(III)
    IF FILE%=4 THEN KY# = KYA4#(III)
    IF DBG>0 THEN PRINT " KEY("III;") is ";FNEKEY$(KY#);KY#,"Range:";I;II
    IF KY# = 0 OR KY# = PKY# THEN 440
    IF PKY# > KY# THEN I=III+1 ELSE II = III
    IF I<II THEN 430
    III=I
440 GET #FILE%,III: REM PRINT I,II,III
    KY#=FNPKEY#(ID$)
    IF DBG > 0 THEN PRINT "Read record";III,"Key is ";ID$;"  packed:";KY#
    IF FILE%=2 THEN KYA2#(III) = KY#
    IF FILE%=3 THEN KYA3#(III) = KY#
    IF FILE%=4 THEN KYA4#(III) = KY#
    IF PKY# = KY# OR I >= II THEN 475
    IF PKY# > KY# THEN I=III+1 ELSE II=III
    GOTO 430
475 REM Found the correct record
    IDENT$(WAYPT)=ID$
    ELEV(WAYPT)=CVI(ELEV$)
    IF FILE%=4 THEN FREQ$(WAYPT)="" ELSE GOSUB UNPKFREQ:FREQ$(WAYPT)=I$
    MV(WAYPT)=ASC(DEV$)-128
    GOSUB 500
    TYPE$(WAYPT)=APT$
    CTY$(WAYPT)=CITY$
    ST$(WAYPT)=STATE$
    WD(WAYPT)=0:WS(WAYPT)=0

    FUELP(WAYPT) = 0:IF FILE%=3 THEN FUELP(WAYPT) = CVI(FUEL$)/1000

490 IF DBG>0 THEN PRINT "Returning '";ID$;"' for requested ";II$:INPUT"Continue Debug";YUP$
    RETURN

    REM Save a little bit of redundant code
499 WAYPT=WAYPT+1
    I$=II$
    GOSUB 400
    WAYPT=WAYPT-1
    RECNUM=III
    RECID$=ID$: REM save the original record ID to see if sort is needed
    RETURN

500 REM Convert/Unpack Latitude & Longitude from the database
    REM Latitude ranges +- 90 degrees
    LADEG(WAYPT)=ASC(LADEG$)-90
      LAMIN(WAYPT)=ASC(LAMIN$)+ASC(LASEC$)/60.

    REM As Longitude ranges +-180 degrees, the high order part is packed
    REM   in with the seconds
    ILOSEC=INT(ASC(LOSEC$)/2)
    ILODEG=ASC(LODEG$)+256*(ASC(LOSEC$)-(2*ILOSEC))
    ILOMIN=ASC(LOMIN$)
    LODEG(WAYPT)=ILODEG-180
      LOMIN(WAYPT)=ASC(LOMIN$)+INT(ASC(LOSEC$)/2)/60.
    RETURN


1000 REM open database files   (Note, this will alter FILE%)
     REM Assignments are:
     REM   #1 Miscellaneous INPUT/OUTPUT
     REM   #2 NAVAIDS
     REM   #3 AIRPORTS
     REM   #4 FIXES
     REM   #5 AIRPORT DETAILS
     OUTPUT$="SCRN:"
     DEXT$=".DAT"

     REM Don't destroy the existing FILE% value
     III = FILE%

     FILE%=2:RECSIZE(FILE%)=59
     FN$(FILE%)="NAVAIDS"
     OPEN "R",#FILE%,FN$(FILE%)+DEXT$,RECSIZE(FILE%)
     NREC(FILE%) = LOF(FILE%)/RECSIZE(FILE%)
     KEYSIZE(FILE%)=5
     GOSUB 1120

     FILE%=3:RECSIZE(FILE%)=84
     FN$(FILE%)="AIRPORTS"
     OPEN "R",#FILE%,FN$(FILE%)+DEXT$,RECSIZE(FILE%)
     NREC(FILE%) = LOF(FILE%)/RECSIZE(FILE%)
     KEYSIZE(FILE%)=4
     GOSUB 1130

     FILE%=4:RECSIZE(FILE%)=40
     FN$(FILE%)="FIXES"
     OPEN "R",#FILE%,FN$(FILE%)+DEXT$,RECSIZE(FILE%)
     NREC(FILE%) = LOF(FILE%)/RECSIZE(FILE%)
     KEYSIZE(FILE%)=5
     GOSUB 1140

     APTDTL%=5
     RECSIZE(APTDTL%) = 125
     FN$(APTDTL%)="AIRPORTS.DTL"
     OPEN "R",#APTDTL%,FN$(APTDTL%),RECSIZE(APTDTL%)
     FIELD #APTDTL%,RECSIZE(APTDTL%) AS DB$

     FILE% = III
     RETURN

1120 REM Set field definitions for Navaids
     FILE%=2
     FIELD #FILE%,KEYSIZE(FILE%) AS ID$,1 AS LADEG$,1 AS LAMIN$,1 AS LASEC$,1 AS LODEG$, 1 AS LOMIN$,1 AS LOSEC$,20 AS APT$,15 AS CITY$,2 AS STATE$, 4 AS REVDATE$,1 AS DEV$,2 AS ELEV$,4 AS FREQ$
     RETURN

1130 REM Set field definitions for Airports
     FILE%=3
     FIELD #FILE%,KEYSIZE(FILE%) AS ID$,1 AS LADEG$,1 AS LAMIN$,1 AS LASEC$,1 AS LODEG$, 1 AS LOMIN$,1 AS LOSEC$,35 AS APT$,20 AS CITY$,2 AS STATE$, 4 AS REVDATE$,1 AS DEV$,2 AS ELEV$,4 AS FREQ$, 2 AS FUEL$, 4 AS APTINFO$
     RETURN

1140 REM Set field definitions for Fixes
     FILE%=4
     FIELD #FILE%,KEYSIZE(FILE%) AS ID$,1 AS LADEG$,1 AS LAMIN$,1 AS LASEC$,1 AS LODEG$, 1 AS LOMIN$,1 AS LOSEC$,15 AS APT$,2 AS STATE$, 4 AS REVDATE$, 1 AS DEV$, 3 AS REFID$, 2 AS RADIAL$, 2 AS DIST$
     CITY$=""
     FREQ$=""
     RETURN

FMTNAV:REM Format a Navagational AID for display
      GOSUB UNPKFREQ:TWR$=I$
      I$=STR$(ASC(DEV$)-128):GOSUB PARSEMAGV:Rem format the VAR$ (Magnetic Variation)
      NAM$=APT$
      ST$=STATE$
      CTY$=CITY$
      ELE$=RIGHT$("      "+STR$(CVI(ELEV$)),6)

      I$ = REVDATE$:GOSUB FMTREVDATE:CYCLE$=II$:REM Format the Revision Cycle Date

      GOSUB FMTCOORD:

      RETURN

FMTAPT: REM Format an Airport for display
        GOSUB UNPKFREQ:TWR$=I$
        I$=STR$(ASC(DEV$)-128):GOSUB PARSEMAGV:Rem format the VAR$ (Magnetic Variation)
        NAM$=APT$
        ST$=STATE$
        CTY$=CITY$
        ELE$=RIGHT$("      "+STR$(CVI(ELEV$)),6)

        GOSUB FMTCOORD:

        FBO$ = STR$(CVI(FUEL$))
        IF LEN(FBO$) > 3 THEN FBO$=LEFT$(FBO$,LEN(FBO$)-3)+"."+RIGHT$(FBO$,3)
        IF CVI(FUEL$) = 0 THEN FBO$="" ELSE IF LEFT$(FBO$,1)=" " THEN FBO$=MID$(FBO$,2)
        IF RIGHT$(FBO$,1) = "0" THEN FBO$=LEFT$(FBO$,LEN(FBO$)-1)

        I$ = REVDATE$:GOSUB FMTREVDATE:CYCLE$=II$:REM Format the Revision Cycle Date

        GOSUB FMTAPTDTL
        RETURN

FMTAPTDTL: REM Handle the fields from the Airport Information Extension file
        ATIS$="":GROUND$="":APPROACH$="":RNWY$="":FBONAME$=""
        TFREQS$="":FUELINFO$=FBO$+FBONAME$
        AINDX& = CVL(APTINFO$)
        IF AINDX& < 1 THEN RETURN

        REM Look up the extended airport information in the extension file

        REM Get the buffers that the record spans
        I = INT(AINDX&/RECSIZE(APTDTL%)):II = AINDX& - (I * RECSIZE(APTDTL%))

        GET #APTDTL%,I+1:II$=DB$
        IF II > 0 THEN II$=MID$(II$,II+1)

        REM Read all of the record into the string buffer
        II = 1
        WHILE INSTR(1,II$,CHR$(13)) = 0
          II = II + 1
          GET #APTDTL%, I+II
          II$=II$+DB$
        WEND

        REM Sanity check the extra detail record
        IF MID$(II$,2,LEN(ID$)) <> ID$ THEN RETURN

        REM Extract the fields from the buffers
        III$=CHR$(34)+","+CHR$(34)
        I = INSTR(1,II$,III$)+LEN(III$)
        II = INSTR(I,II$,III$)
        TFREQS$ = MID$(II$,I,II-I)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        ATIS$ = MID$(II$,1,II-1)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        GROUND$ = MID$(II$,1,II-1)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        APPROACH$ = MID$(II$,1,II-1)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        RNWY$ = MID$(II$,1,II-1)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        FBONAME$ = MID$(II$,1,II-1)

        IF FBO$ = "" THEN FUELINFO$=FBONAME$ ELSE FUELINFO$=FBO$+" "+FBONAME$

        RETURN

FMTFIX: REM Format a Navigational FIX for displaying
        I$=STR$(ASC(DEV$)-128):GOSUB PARSEMAGV:Rem format the VAR$ (Magnetic Variation)
        NAM$=APT$
        ST$=STATE$
        CTY$=CITY$

        GOSUB FMTCOORD:
        REF1$=REFID$
        RADIAL=CVI(RADIAL$)
        DIST=CVI(DIST$)/10

        I$ = REVDATE$:GOSUB FMTREVDATE:CYCLE$=II$:REM Format the Revision Cycle Date

        RETURN

REM *************************************************************************
REM   EVERYTHING BEFORE THIS POINT IS IN COMMON WITH ALSE6B & ALSE6BCF
REM   CHANGES SHOULD BE DUPLICATED IN BOTH MODULES (Or Block Copied)
REM *************************************************************************

12000 REM Prompt for a default FAA Revision Cycle date to be used
      WHILE LEN(FAACYCLE$) <> 10
         PRINT "The FAA revision cycle date for this update is unknown."
         INPUT "Enter the cycle date (mm/dd/yyyy) to be used as a default";I$
         IF LEN(I$) = 10 THEN
            GOSUB PACKREVDATE
            I$=II$:GOSUB FMTREVDATE
            PRINT "You entered";II$;:INPUT "Press ENTER to continue";I$
            IF I$ = "" THEN FAACYCLE$=II$
         ELSE PRINT"The format to enter is specific.  MM/DD/YYYY"
         ENDIF
      WEND
      RETURN

13000 REM Aircraft Weight and Balance specification
      CHANGE$="FALSE"
13010 CLS:PRINT TAB(30);"Basic E6B Calculations"
      PRINT TAB(25);"Aircraft Weight and Balance Specifications"
      PRINT TAB((72-LEN(PROFILE$))/2);"Using: ";PROFILE$
      PRINT
      PRINT USING" 1 - Base weight: #####.#";EMPTYWT
      PRINT USING" 2 - Base CG:       ###.##";EMPTYCG
      PRINT" 3 - Work with the envelope specification"
      PRINT" 4 - Work with categories"
      PRINT" 5 - Work with items within a category"
      PRINT
      PRINT" 6 - Add/Remove equipment from base empty weight"
      PRINT
      INPUT"selection";I
      IF I > 6 THEN 13010
      IF I > 0 THEN ON I GOSUB 13200,13210,13300,13400,13500,13600:GOTO 13010

      IF CHANGE$="FALSE" THEN RETURN
      PRINT:INPUT"Save changes (Y/N)";I$
      IF LEFT$(I$,1) <> "Y" AND LEFT$(I$,1) <> "y" THEN RETURN

      I$ = PROFILE$
      GOSUB 15170
      GOSUB 13100
      RETURN

13100 REM Read Weight and Balance Specifications
      GOSUB 80
      IF STATUS$<>"OK" THEN RETURN
      GOSUB 85
      ON ERROR GOTO 13190

      INPUT #1,EMPTYWT,EMPTYCG

      INPUT #1,NPTS
      DMPTS = NPTS + 5
      REDIM XY(DMPTS,2)  :REM Number of data points on the envelope curve
      MINWT = 1E14: MAXWT = 0
      MINCG = 99999:MAXCG=0
      FOR I = 1 to NPTS
         INPUT#1,XY(I,0),XY(I,1)
         IF XY(I,0) < MINCG THEN MINCG = XY(I,0)
         IF XY(I,0) > MAXCG THEN MAXCG = XY(I,0)
         IF XY(I,1) < MINWT AND XY(I,1) > 0 THEN MINWT = XY(I,1)
         IF XY(I,1) > MAXWT THEN MAXWT = XY(I,1)
      NEXT I

      INPUT #1,NCATS
      DMCATS = NCATS+10:  REM Make the array a little bigger for editing
      REDIM CAT$(DMCATS),NITEMS(DMCATS),ITEMINX(DMCATS)
      TITEMS = 0
      IF NCATS = 0 THEN
         REM Provide some reasonable defaults
         NCATS = NCATS+1:CAT$(NCATS) = "Seats"
         NCATS = NCATS+1:CAT$(NCATS) = "Fuel"
         NCATS = NCATS+1:CAT$(NCATS) = "Baggage"
         FOR I = 1 TO NCATS
             NITEMS(I)=0
             ITEMINX(I)=0
         NEXT I
      ELSE
         FOR I = 1 TO NCATS
             INPUT #1,CAT$(I),NITEMS(I)
             ITEMINX(I)=0
             TITEMS = TITEMS + NITEMS(I)
         NEXT I
      ENDIF
      ACTIVE = 0
      IF NCATS >= 1 THEN ACTIVE = 1

      REM    Name of item, Default val,  lbs Conversion, moment arm,  max val     ,Value
      DMITEMS = TITEMS + 20
      REDIM ITEM$(DMITEMS), DFV(DMITEMS),CV(DMITEMS),    ARM(DMITEMS),MXV(DMITEMS),V(DMITEMS)

      II = 1
      FOR I = 1 TO NCATS
          ITEMINX(I) = II
          FOR J = 1 TO NITEMS(I)
              INPUT #1,ITEM$(II),DFV(II),CV(II),ARM(II),MXV(II)
              V(II) = DFV(II)
              II = II + 1
          NEXT J
      NEXT I

13170 ON ERROR GOTO 0
      CLOSE #1
13180 RETURN

13190 RESUME 13170

13200 PRINT
      PRINT" This is the 'empty' weight for calculations purposes."
      PRINT" This should include any installed equipment, etc."
      PRINT" However, don't include unusable fuel."
      PRINT"Enter new 'empty' weight (in lbs.)";
      IF EMPTYWT > 0 THEN PRINT", or ENTER to keep the same";
      INPUT J
      IF J > 0 then EMPTYWT = J:CHANGE$="TRUE"
      RETURN

13210 PRINT
      PRINT" This is Center of Gravity (arm) for the base 'empty' weight."
      PRINT"Enter new CG (in inches)";
      IF EMPTYCG > 0 THEN PRINT", or ENTER to keep";EMPTYCG;
      INPUT J
      IF J > 0 then EMPTYCG = J:CHANGE$="TRUE"
      RETURN

13300 CLS:PRINT TAB(29)"Weight and Balance Envelope"
      PRINT
      PRINT" Enter points to describe a typical envelope as shown below:"
      PRINT
      PRINT"             D       E"
      PRINT"        C             "
      PRINT"      B               "
      PRINT"                      "
      PRINT"      A              F"
      PRINT
      PRINT" 1 - Delete a point"
      PRINT" 2 - Insert a point before..."
      PRINT" 3 - Add a point after..."
      PRINT" 4 - Modify a point..."
      PRINT
      FOR I = 1 TO NPTS
         PRINT TAB(7);CHR$(ASC("A")+I-1);" - ";XY(I,0);",";XY(I,1)
      NEXT I
      PRINT
      INPUT "Selection";J
      IF J = 0 THEN RETURN
      IF J > 4 THEN 13300
      IF J = 1 AND NPTS < 1 THEN 13300
      CHANGE$="TRUE"
      IF NPTS >= 1 THEN
         IF J = 1 THEN PRINT "Delete ";
         IF J = 2 THEN PRINT "Insert before ";
         IF J = 3 THEN PRINT "Add after ";
         PRINT "reference point ( A .. ";CHR$(ASC("A")+NPTS-1);" )";
         INPUT I$
         I = ASC(I$)
         IF I > ASC("Z") THEN I = I - (ASC("a")-ASC("A"))
         I = I - ASC("A") + 1
         IF I > NPTS OR I < 1 THEN 13300
      ELSE
         I = 0
         J = 3
      ENDIF

      IF J = 1 THEN
         FOR JJ = I TO NPTS
             XY(JJ,0) = XY(JJ+1,0)
             XY(JJ,1) = XY(JJ+1,1)
         NEXT JJ
         NPTS = NPTS - 1
      ELSE
         IF J = 3 THEN I = I + 1
         PRINT "Enter coordinate pair ";I;
         INPUT " (0,0 to ignore)";x,y
         IF X = 0 AND Y = 0 THEN 13300
         IF J < 4 THEN
            FOR JJ = NPTS TO I STEP -1
                XY(JJ+1,0) = XY(JJ,0)
                XY(JJ+1,1) = XY(JJ,1)
            NEXT JJ
            NPTS = NPTS + 1
         ENDIF
         XY(I,0)=X
         XY(I,1)=Y
      ENDIF

      GOTO 13300

13400 CLS:PRINT TAB(32)"Weight and Balance Categories"
      PRINT
      PRINT" 1 - Delete a category"
      PRINT" 2 - Insert a category before..."
      PRINT" 3 - Add a category after..."
      PRINT" 4 - Modify a category..."
      PRINT
      FOR I = 1 TO NCATS
         PRINT TAB(7);CHR$(ASC("A")+I-1);" - ";CAT$(I)
      NEXT I
      PRINT
      INPUT "Selection";J
      IF J = 0 THEN RETURN
      IF J > 4 THEN 13400
      IF J = 1 AND NCATS < 1 THEN 13400
      CHANGE$="TRUE"
      IF NCATS >= 1 THEN
         IF J = 1 THEN PRINT "Delete ";
         IF J = 2 THEN PRINT "Insert before ";
         IF J = 3 THEN PRINT "Add after ";
         PRINT "reference category ( A .. ";CHR$(ASC("A")+NCATS-1);" )";
         INPUT I$
         I = ASC(I$)
         IF I > ASC("Z") THEN I = I - (ASC("a")-ASC("A"))
         I = I - ASC("A") + 1
         IF I > NCATS OR I < 1 THEN 13400
      ELSE
         I = 0
         J = 3
      ENDIF

      IF J = 1 THEN
         FOR JJ = I TO NCATS
             CAT$(JJ) = CAT$(JJ+1)
             NITEMS(JJ) = NITEMS(JJ+1)
             ITEMINX(JJ) = ITEMINX(JJ+1)
         NEXT JJ
         NCATS = NCATS - 1
      ELSE
         IF J = 3 THEN I = I + 1
         PRINT "Category description ";CHR$(ASC("A")+I-1);
         INPUT " (ENTER to ignore)";I$
         IF I$ = "" THEN 13400
         IF J < 4 THEN
            FOR JJ = NCATS TO I STEP -1
                CAT$(JJ+1) = CAT$(JJ)
                NITEMS(JJ+1) = NITEMS(JJ)
                ITEMINX(JJ+1) = ITEMINX(JJ)
            NEXT JJ
            NCATS = NCATS + 1
         ENDIF
         CAT$(I)=I$
         NITEMS(I)=0
      ENDIF

      GOTO 13400

13500 CLS:PRINT TAB(32)"Weight and Balance Items"
      PRINT
      PRINT" 1 - Select active category (";CAT$(ACTIVE);")"
      PRINT
      PRINT" 2 - Delete an item"
      PRINT" 3 - Insert an item before..."
      PRINT" 4 - Add an item after..."
      PRINT" 5 - Modify an item..."
      PRINT
      PRINT TAB(11);"ITEM                      ARM     Conv Factor   Default  Maximum"
      FOR I = 1 TO NITEMS(ACTIVE)
         JJ = ITEMINX(ACTIVE)+I-1
         IF CV(JJ) = 0 THEN CV(JJ) = 1
         PRINT USING "\\- \                      \ ###.##   ###.##        ####.#  ####.#";TAB(7);CHR$(ASC("A")+I-1);ITEM$(JJ),ARM(JJ),CV(JJ),DFV(JJ)/CV(JJ),MXV(JJ)/CV(JJ)
      NEXT I
      PRINT
      INPUT "Selection";J
      IF J = 0 THEN RETURN
      IF J = 1 THEN
         PRINT
         FOR I = 1 TO NCATS
             PRINT I;" - ";CAT$(I)
         NEXT I
         PRINT:INPUT "Select active category";I
         IF I>0 AND I <= NCATS THEN ACTIVE = I
         GOTO 13500
      ENDIF
      IF J > 5 THEN 13500
      IF J = 2 AND NITEMS(ACTIVE) < 1 THEN 13500
      CHANGE$="TRUE"
      IF NITEMS(ACTIVE) >= 1 THEN
         IF J = 2 THEN PRINT "Delete ";
         IF J = 3 THEN PRINT "Insert before ";
         IF J = 4 THEN PRINT "Add after ";
         DO UNTIL I <= NITEMS(ACTIVE) AND I >= 1
            PRINT "reference item ( A .. ";CHR$(ASC("A")+NITEMS(ACTIVE)-1);" )";
            INPUT I$
            IF I$ = "" THEN 13500
            IF LEN(I$) = 1 THEN
               I = ASC(I$)
               IF I > ASC("Z") THEN I = I - (ASC("a")-ASC("A"))
               I = I - ASC("A") + 1
            ENDIF
         LOOP
      ELSE
         I = 0
         J = 4
      ENDIF

      I = ITEMINX(ACTIVE)+I-1
      IF J = 2 THEN
         FOR JJ = I TO TITEMS
             ITEM$(JJ) = ITEM$(JJ+1)
             CV(JJ)    = CV(JJ+1)
             DFV(JJ)   = DFV(JJ+1)
             ARM(JJ)   = ARM(JJ+1)
             MXV(JJ)   = MXV(JJ+1)
         NEXT JJ
         FOR JJ = ACTIVE+1 TO NCATS
             ITEMINX(JJ) = ITEMINX(JJ) - 1
         NEXT JJ
         NITEMS(ACTIVE) = NITEMS(ACTIVE) - 1
         TITEMS = TITEMS - 1
      ELSE
         IF J = 4 THEN I = I + 1
         PRINT "Item description ";
         IF J = 5 THEN PRINT "(";ITEM$(I);") (ENTER for no change)";
         INPUT I$
         IF I$ = "" AND J <> 5 THEN 13500

         PRINT "Enter the arm";
         IF J = 5 THEN PRINT "(";ARM(I);")";
         INPUT " in inches";ARM

         CV = CV(I):IF CV = 0 THEN CV = 1
         PRINT "Enter a conversion factor (";CV;") to lbs";
         INPUT CV

         PRINT "Enter the default value";
         IF J = 5 AND CV(I) > 0 THEN PRINT USING "(####.#)";DFV(I)/CV(I);
         INPUT DFV

         PRINT "Enter the maximum allowable value";
         IF J = 5 AND CV(I) > 0 THEN PRINT USING "(####.#)";MXV(I)/CV(I);
         INPUT MXV

         IF J < 5 THEN
            REM Make room for the new item by rippling everything down
            FOR JJ = TITEMS TO I STEP -1
                ITEM$(JJ+1) = ITEM$(JJ)
                CV(JJ+1)    = CV(JJ)
                DFV(JJ+1)   = DFV(JJ)
                ARM(JJ+1)   = ARM(JJ)
                MXV(JJ+1)   = MXV(JJ)
            NEXT JJ
            FOR JJ = ACTIVE+1 TO NCATS
                ITEMINX(JJ) = ITEMINX(JJ) + 1
            NEXT JJ
            NITEMS(ACTIVE) = NITEMS(ACTIVE) + 1
            TITEMS = TITEMS + 1
         ENDIF
         IF I$ <> "" THEN ITEM$(I)=I$
         IF CV > 0 THEN CV(I)=CV
         IF CV(I) = 0 THEN CV(I) = 1
         IF DFV <> 0 THEN DFV(I)=DFV*CV(I)
         IF ARM <> 0 THEN ARM(I)=ARM
         IF MXV <> 0 THEN MXV(I)=MXV*CV(I)
      ENDIF

      GOTO 13500

13600 REM Compute new empty weight based on equipment changes
      LOGF$=""
13610 CLS:PRINT TAB(30);"Basic E6B Calculations"
      PRINT TAB(27);"Recompute Empty Weight and CG"
      PRINT
      PRINT" A - Add equipment"
      PRINT" R - Remove equipment"
      IF LOGF$="" THEN PRINT" L - Create a log file"
      PRINT
      PRINT USING " Computed empty weight:   #####.# ";EMPTYWT
      PRINT USING " Computed Center of Gravity ###.##";EMPTYCG
      PRINT
      INPUT "Selection";I$
      IF I$="" THEN
         IF LOGF$<>"" THEN CLOSE #1
         RETURN
      ENDIF
      IF LEFT$(I$,1) = "L" OR LEFT$(I$,1) = "l" THEN
         PRINT
         INPUT"To create a calculation log, ENTER the name of a file";LOGF$
         IF LOGF$<>"" THEN OPEN "O",#1,LOGF$
      ELSE
         IF LEFT$(I$,1) = "A" OR LEFT$(I$,1) = "a" THEN
            GOSUB 13630
         ELSE
            IF LEFT$(I$,1) = "R" OR LEFT$(I$,1) = "r" THEN GOSUB 13650
         ENDIF
      ENDIF
      GOTO 13610
      RETURN

13630 REM Add equipment
      PRINT USING " Computed empty weight:   #####.# ";EMPTYWT
      PRINT USING " Computed Center of Gravity ###.##";EMPTYCG
      PRINT
      IF LOGF$<>"" THEN INPUT"Equipment description";II$
      INPUT "Added weight (0 to quit)";WT
      IF WT = 0 THEN RETURN
      INPUT "   Arm (0 to respecify weight)";ARM
      IF ARM = 0 THEN 13630
      CHANGE$="TRUE"
      IF LOGF$<>"" THEN PRINT #1,USING "Weight before #####.#, CG ###.##";EMPTYWT;EMPTYCG;
      TOTMOM! = (EMPTYWT * EMPTYCG) + (WT * ARM)
      EMPTYWT = EMPTYWT + WT
      EMPTYCG = TOTMOM!/EMPTYWT
      IF LOGF$<>"" THEN PRINT #1,USING "   New Weight #####.#, CG ###.##";EMPTYWT;EMPTYCG;
      IF LOGF$<>"" THEN PRINT #1,"Adding: ";II$
      GOTO 13630

13650 REM Remove equipment
      PRINT USING " Computed empty weight:   #####.# ";EMPTYWT
      PRINT USING " Computed Center of Gravity ###.##";EMPTYCG
      PRINT
      IF LOGF$<>"" THEN INPUT"Equipment description";II$
      INPUT "Weight removed (0 to quit)";WT
      IF WT = 0 THEN RETURN
      INPUT "   Arm (0 to respecify weight)";ARM
      IF ARM = 0 THEN 13650
      CHANGE$="TRUE"
      IF LOGF$<>"" THEN PRINT #1,USING "Weight before #####.#, CG ###.##";EMPTYWT;EMPTYCG;
      TOTMOM! = (EMPTYWT * EMPTYCG) - (WT * ARM)
      EMPTYWT = EMPTYWT - WT
      EMPTYCG = TOTMOM!/EMPTYWT
      IF LOGF$<>"" THEN PRINT #1,USING "   New Weight #####.#, CG ###.##";EMPTYWT;EMPTYCG;
      IF LOGF$<>"" THEN PRINT #1,"Remove: ";II$
      GOTO 13650

13700 REM Aircraft Weight and Balance Calculation
      CLS:PRINT TAB(30);"Basic E6B Calculations"
      PRINT TAB(22);"Aircraft Weight and Balance Calculations"

      GROSSWT = EMPTYWT
      TOTMOM! = EMPTYCG * EMPTYWT

      IF TOTMOM! = 0 OR TITEMS = 0 OR NPTS = 0 THEN
         PRINT
         PRINT"You must first define the parameters that I can use to calculate!"
         INPUT"Press ENTER to continue";I$
         IF TOTMOM! = 0 THEN IF EMPTYWT = 0 THEN 13200 ELSE GOTO 13210
         IF NPTS = 0 THEN 13300 ELSE GOTO 13500
         GOTO 13000
      ENDIF

      J = 0
      FOR I = 1 TO NCATS
         IF NITEMS(I) > 0 THEN
            PRINT:PRINT CAT$(I)
            JJ = ITEMINX(I) - 1
            FOR II = 1 TO NITEMS(I)
                TS = ((II-1)-INT((II-1)/2)*2)*40 + 3  :REM Dual columns tab stop
                J = J + 1
                GROSSWT = GROSSWT + V(JJ+II)
                TOTMOM! = TOTMOM! + (ARM(JJ+II) * V(JJ+II))
                PRINT USING "## - \                      \ ####.#";TAB(TS);J;ITEM$(JJ+II);V(JJ+II)/CV(JJ+II);
            NEXT II
         ENDIF
      NEXT I
      PRINT
      PRINT
      CG = TOTMOM!/GROSSWT
      PRINT USING "Total weight:    ####.#        Center of Gravity ###.##";GROSSWT;CG
      PRINT USING "Extremes: ####.#  to  ####.#              ###.##   to   ###.##";MINWT;MAXWT;MINCG;MAXCG

      REM Now compute how where we are in the appropriate envelope range
      JJ = NPTS
      FOR I = NPTS TO 0 STEP -1
          IF CG < XY(I+1,0) THEN JJ = I
      NEXT I
      I$=""
      III$="Curve segment: "+CHR$(ASC("A")+JJ-1)+"-"+CHR$(ASC("A")+JJ)+":"
      IF CG < XY(JJ,0) OR CG > XY(JJ+1,0) THEN PRINT " **** CG is out of range":I$=CHR$(7)
      IF GROSSWT > XY(JJ+1,1) THEN
         PRINT " **** Over weight":I$=CHR$(7)
      ENDIF

      REM Compute the allowable gross weight equation for this part of envelope
      IF I$ = "" THEN
         M = (XY(JJ+1,1) - XY(JJ,1)) / (XY(JJ+1,0) - XY(JJ,0))
         B = XY(JJ,1) - (M * XY(JJ,0))
         GROSSOK = (M * CG) + B
         PRINT III$;
         PRINT USING "  ###.##,#####.# to ###.##,#####.#    Allowable gross #####.#";XY(JJ,0);XY(JJ,1);XY(JJ+1,0);XY(JJ+1,1);GROSSOK
         IF GROSSWT > GROSSOK THEN
            PRINT USING " **** Over weight for the actual CG by ####.# lbs";GROSSWT-GROSSOK:I$=CHR$(7)
         ELSE
            PRINT USING "  under gross weight by ####.# lbs";GROSSOK-GROSSWT
         ENDIF
      ENDIF

      PRINT I$
      INPUT"selection";JJ
      IF JJ = 0 THEN RETURN
      IF JJ > J THEN 13700

      PRINT:PRINT "Enter new value for ";ITEM$(JJ);
      INPUT I$
      IF I$ = "" THEN 13700
      V(JJ) = VAL(I$) * CV(JJ)
      GOTO 13700

14000 REM Aircraft Performance specification
      CHANGE$="FALSE"
14010 CLS:PRINT TAB(30);"Basic E6B Calculations"
      PRINT TAB(31);"Aircraft Performance"
      PRINT
      PRINT" 1 - True airspeed at cruise:";KTS
      PRINT" 2 - Desired approximate cruising altitude:";CALT
      PRINT" 3 - Using";FPM;"FPM climb at";INT(CLIMBSP*KTS+.5);"KTS over the ground."
      PRINT"          (from departure elevation to";CALT;"feet.)
      PRINT
      PRINT" 4 - Departure fuel load";FUELLOAD;"(gallons)"
      PRINT" 5 - Fuel capacity";FUELCAP;"(gallons)"
      PRINT" 6 - Minimum fuel for comfort";FUELMIN;"(gallons)"
      PRINT" 7 - Cruise fuel burn rate";FUELBURN;"(gal/hr)"
      PRINT" 8 - Hourly incremental cost";HOURLY;"($$/hr)"
      PRINT
      PRINT" 9 - Route corridor width is";INT((QINCREASE - 1) * 100);"% of route distance"
      PRINT"10 - Minimum search corridor width is";QROUTEWIDTH;"nm"
      PRINT
      PRINT"11 - Default (No wind) runway heading is";RUNWYHD
      PRINT
      INPUT"selection";I
      IF I = 0 THEN 14100
      IF I <= 11 THEN ON I GOSUB 14110,14120,14130,14200,14210,14220,14230,14240,14150,14160,14170
      GOTO 14010

14100 IF CHANGE$="TRUE" THEN GOSUB 15160
      RETURN

14110 PRINT
      PRINT "True airspeed at cruise defaults to";KTS
      INPUT "Enter different setting (or ENTER to keep same value)";I
      IF I > 0 THEN KTS = I:CHANGE$="TRUE"
      RETURN

14120 PRINT
      PRINT"Average cruising altitude";CALT
      INPUT"Enter different cruising altitude (or ENTER to keep same value)";I
      IF I > 0 THEN CALT=I:CHANGE$="TRUE"
      RETURN

14130 PRINT
      PRINT"This is used to compute how long it will take you to reach your cruising"
      PRINT"altitude, and how far you will have gone in that time.  This is the no wind"
      PRINT"ground speed and not the indicated airspeed."
      PRINT"using";FPM;"FPM climb at";INT(CLIMBSP*KTS+.5);"KTS.  (from departure elevation to";CALT;"feet.)
      PRINT"Enter a new average CLIMB RATE (or ENTER to keep";FPM;"FPM)";:INPUT I
      IF I > 0 THEN FPM = I:CHANGE$="TRUE"

14140 PRINT
      PRINT"Enter a new cruise CLIMB SPEED (or ENTER to keep";INT(CLIMBSP*KTS+.5);"KTS)";:INPUT I
      IF I > 0 THEN CLIMBSP = I/KTS:CHANGE$="TRUE"
      RETURN

14150 PRINT
      PRINT"This is maximum allowable increase you are willing to deviate from"
      PRINT"the straight line course.  For waypoints that are not near the departure,"
      PRINT"intermediate point(s), or destination to be considered, the routing"
      PRINT"via the potential waypoint must not increase the distance by this much."
      PRINT"Route corridor width is";INT((QINCREASE - 1) * 100);"% of route distance"
      INPUT"Enter new routing factor, or ENTER to keep same value";I
      IF I > 0 THEN QINCREASE = 1 + (I/100):CHANGE$="TRUE"

14160 PRINT
      PRINT"This is the radius that will qualify potential waypoints around"
      PRINT"a certain position.  For example, it is used to search for airports"
      PRINT"around the departure, destination, and intermediate waypoints."
      PRINT"Minimum search corridor width is";QROUTEWIDTH
      INPUT"Enter new minimum corridor width, or ENTER to keep same value";I
      IF I > 0 THEN QROUTEWIDTH = I:CHANGE$="TRUE"
      RETURN

14170 PRINT
      PRINT"Default destination runway heading is";RUNWYHD
      INPUT"Enter a new default runway heading or ENTER to keep same value";I
      IF I > 0 AND I <= 360 THEN RUNWYHD = I:CHANGE$="TRUE"
      RETURN

14200 PRINT
      INPUT"Fuel on board at departure (or ENTER to keep same value)";I
      IF I > 0 THEN FUELLOAD=I:CHANGE$="TRUE"
      RETURN

14210 PRINT
      INPUT"Maximum fuel capacity (or ENTER to keep same value)";I
      IF I > 0 THEN FUELCAP=I:CHANGE$="TRUE"
      RETURN

14220 PRINT
      PRINT"This is your comfort level for remaining fuel.  It may be higher than"
      PRINT"the FAA requirements based on how your plane maintains unuseable fuel"
      PRINT"or what you feel your personal reserves are."
      PRINT "Comfortable minimum fuel level (or ENTER to keep";FUELMIN;")";
      INPUT I
      IF I > 0 THEN FUELMIN=I:CHANGE$="TRUE"
      RETURN

14230 PRINT
      INPUT"New cruise fuel burn rate (Gal/hr) (or ENTER to keep same value)";I
      IF I > 0 THEN FUELBURN=I:CHANGE$="TRUE"
      RETURN

14240 PRINT
      PRINT"This is your allowance for items that wear out based on hours."
      PRINT"For example, oil, engine overhaul.  It should not include a cost"
      PRINT"factor for items that are fixed regardless on the number of hours"
      PRINT"flow.  For example, don't include costs of hangar, insurance, etc."
      PRINT USING "New hourly incremental cost ($/hr) (or ENTER to keep $##.##)";HOURLY
      INPUT I
      IF I > 0 THEN HOURLY=I:CHANGE$="TRUE"
      RETURN

      REM Change Default Cruise configuration file name
14400 INPUT"File name for default cruise characteristics";I$:IF I$<>"" THEN PROFILE$=I$:GOSUB 14650
      RETURN

      REM Printer configuration
14500 CHANGE$="FALSE":REM alter the printer configuration escape sequence codes
14505 CLS:PRINT TAB(30);"Basic E6B Calculations"
      PRINT TAB(22);"Printer escape sequence configuration":PRINT
      PRINT"1 - Initialization (compressed print) in decimal numbers":PRINT TAB(15);
      FOR I=1 TO LEN(PRINIT$):PRINT ASC(MID$(PRINIT$,I,1));:NEXT I:PRINT:PRINT
      PRINT"2 - Superscript (1/2 up tiny characters for Latitude)":PRINT TAB(15);
      FOR I=1 TO LEN(PRSUPER$):PRINT ASC(MID$(PRSUPER$,I,1));:NEXT I:PRINT:PRINT
      PRINT"3 - Subscript (1/2 down tiny characters for Longitude)":PRINT TAB(15);
      FOR I=1 TO LEN(SUB$):PRINT ASC(MID$(SUB$,I,1));:NEXT I:PRINT:PRINT
      PRINT"4 - Normal base line characters":PRINT TAB(15);
      FOR I=1 TO LEN(NORMAL$):PRINT ASC(MID$(NORMAL$,I,1));:NEXT I:PRINT:PRINT
      PRINT"5 - Degree character  (";DEGSYM$(0);")":PRINT TAB(15);
      FOR I=1 TO LEN(DEGSYM$(1)):PRINT ASC(MID$(DEGSYM$(1),I,1));:NEXT I:PRINT:PRINT
      PRINT
      INPUT"selection";I
      IF I = 0 THEN 14600
      IF I<0 OR I> 5 THEN PRINT CHR$(7):GOTO 14500
      ON I GOSUB 14580,14582,14584,14586,14588
      GOTO 14505

      REM Change "Compressed Print" printer control
14580 I$=PRINIT$:GOSUB 14700:IF I$<> PRINIT$ THEN CHANGE$="TRUE"
      PRINIT$=I$
      RETURN

      REM Change "Superscript" printer control
14582 I$=PRSUPER$:GOSUB 14700:IF I$<> PRSUPER$ THEN CHANGE$="TRUE"
      PRSUPER$=I$
      RETURN

      REM Change "Subscript" printer control
14584 I$=SUB$:GOSUB 14700:IF I$ <> SUB$ THEN CHANGE$="TRUE"
      SUB$=I$
      RETURN

      REM Change "return to normal characters" printer control
14586 I$=NORMAL$:GOSUB 14700:IF I$ <> NORMAL$ THEN CHANGE$="TRUE"
      NORMAL$=I$
      RETURN

      REM Change "degrees" symbol  (Printable character)
14588 I$=DEGSYM$(1):GOSUB 14700:IF I$ <> DEGSYM$(1) THEN CHANGE$="TRUE"
      DEGSYM$(1)=I$
      RETURN

14600 REM Confirm and Write Printer Configuration
      IF CHANGE$="FALSE" THEN RETURN
      PRINT"Do you wish to write the changes to ";CONFIG$;:INPUT " (Y/n)";I$
      IF LEFT$(I$,1) <> "Y" AND LEFT$(I$,1) <> "y" THEN RETURN

14650 OPEN "O",#1,CONFIG$

      PRINT #1,PROFILE$
      PRINT #1,DFPRTR$

      PRINT #1,LEN(PRINIT$)
      FOR I=1 TO LEN(PRINIT$):PRINT#1,ASC(MID$(PRINIT$,I,1)):NEXT I

      PRINT #1,LEN(DEGSYM$(1))
      FOR I=1 TO LEN(DEGSYM$(1)):PRINT#1,ASC(MID$(DEGSYM$(1),I,1)):NEXT I

      PRINT #1,LEN(PRSUPER$)
      FOR I=1 TO LEN(PRSUPER$):PRINT#1,ASC(MID$(PRSUPER$,I,1)):NEXT I

      PRINT #1,LEN(SUB$)
      FOR I=1 TO LEN(SUB$):PRINT#1,ASC(MID$(SUB$,I,1)):NEXT I

      PRINT #1,LEN(NORMAL$)
      FOR I=1 TO LEN(NORMAL$):PRINT#1,ASC(MID$(NORMAL$,I,1)):NEXT I

      CLOSE #1
      RETURN

14700 REM get a printer control string
      FOR I=1 TO LEN(I$):PRINT ASC(MID$(I$,I,1));:NEXT I:PRINT
      INPUT"enter new control sequence (decimal values, ~ deletes all)";II$
      IF II$=" " OR II$="~" THEN I$="":RETURN
      IF II$="" THEN II$=I$:RETURN ELSE II$=II$+" "
      I$="":II=1:I=0
14750 I=I+1:IF I >= LEN(II$) THEN 14760
14755 IF MID$(II$,I,1) <> " " AND MID$(II$,I,1) <> "," THEN 14750
14760 III = VAL(MID$(II$,II,I-II))
      IF III < 256 THEN I$=I$+CHR$(III) ELSE PRINT CHR$(7);
14770 IF I<=LEN(II$) THEN IF MID$(II$,I+1,1) = " " THEN I=I+1:GOTO 14770
      IF I < LEN(II$) THEN II=I:GOTO 14750
      RETURN

15160 PRINT
      INPUT"Create/update an aircraft profile with these values (Y/N)";I$
      IF LEFT$(I$,1) <> "Y" AND LEFT$(I$,1) <> "y" THEN RETURN
      PRINT:PRINT"Name of file store the values (default is ";PROFILE$;")";
      INPUT I$
      IF I$="" THEN I$=PROFILE$
15170 OPEN "O",#1,I$
      WRITE #1,KTS,CLIMBSP,FPM,CALT
      WRITE #1,QINCREASE,QROUTEWIDTH
      WRITE #1,FUELLOAD,FUELCAP,FUELMIN,FUELBURN,HOURLY
      WRITE #1,RUNWYHD
      WRITE #1,EMPTYWT,EMPTYCG

      WRITE #1,NPTS
      FOR I = 1 TO NPTS
         WRITE #1,XY(I,0),XY(I,1)
      NEXT I

      WRITE #1,NCATS
      FOR I = 1 TO NCATS
         WRITE #1,CAT$(I),NITEMS(I)
      NEXT I

      FOR I = 1 TO NCATS
          II = ITEMINX(I)
          FOR J = 1 TO NITEMS(I)
              WRITE #1,ITEM$(II),DFV(II),CV(II),ARM(II),MXV(II)
              II = II + 1
          NEXT J
      NEXT I

      CLOSE #1

      IF I$ = PROFILE$ THEN RETURN
      PRINT"Do you want this Performance profile (";I$;") to be the default";
      INPUT " (Y/N)";II$
      IF LEFT$(II$,1) <> "Y" AND LEFT$(II$,1) <> "y" THEN RETURN
      PROFILE$=I$
      GOSUB 14650
      RETURN

20000 REM Database Input/Export functions
      CLS:PRINT SPC(30);"Basic E6B Calculations"
      PRINT TAB(28);"Database Input/Export":PRINT
      PRINT"1 - Update AIRPORTS.DAT with data imported from Airports.INP"
      PRINT"2 - Export Airport information"
      PRINT
      PRINT"3 - Update NAVAIDS.DAT with data imported from Navaids.INP"
      PRINT"4 - Export Navaid information"
      PRINT
      PRINT"5 - Update FIXES.DAT with data imported from Fixes.INP"
      PRINT"6 - Export navigational Fix information"
      PRINT
      PRINT:INPUT"selection";I
      IF I < 1 THEN RETURN
      IF I <=6 THEN ON I GOSUB 24000,24500,25000,25500,26000,26500
      GOTO 20000

20100 REM Invoke the "Data Base Maintenance" program
      REM We will never "return" here, but will re-start this
      REM   program's execution from the beginning...
      CLOSE
      RUN "ALSE6BDB"

24000 REM Import records from AIRPORTS.INP file updating AIRPORTS.DAT
      I$="AIRPORTS.INP"
      PRINT"Import from what file (ENTER=";I$;")";:INPUT II$
      IF II$ <> "" THEN I$=II$
      ON ERROR GOTO 24010
      OPEN "I",#1,I$

      REM Read all the existing records creating keys so we can
      REM check for duplicate/updated entries as we are adding records
      GOSUB 1130
      FOR I = 1 TO NREC(FILE%)
         GET #FILE%,I
         KYA3#(I) = FNPKEY#(ID$)
      NEXT I
      GOTO 24020

24010 RESUME 24011
24011 ON ERROR GOTO 0
      PRINT "Error attempting to open '";I$;"'."
      INPUT "Press ENTER";I$
      RETURN

24020 REM Read an input record from AIRPORTS.INP
      ON ERROR GOTO 24090
      INPUT #1,RECID$,NAM$,CTY$,ST$,LAT$,LONG$,ELE$,VAR$,TFREQS$, ATIS$,GROUND$,APPROACH$,RNWY$,FUELINFO$,CYCLE$
      ON ERROR GOTO 0
      PRINT "Processing ";LEFT$(RECID$+"     ",5);
      DTLCHANGE$ = "TRUE"

      REM Convert the revision date into a binary we can work with
      IF CYCLE$ = "" THEN GOSUB 12000:CYCLE$ = FAACYCLE$
      I$=CYCLE$:GOSUB PACKREVDATE:NEWCYCLE& = J&

      REM See if we are to Update an existing record
      PKY# = FNPKEY#(LEFT$(RECID$+"     ",KEYSIZE(FILE%)))
      RECNUM = 0
      FOR I = 1 TO NREC(FILE%)
          IF KYA3#(I) = PKY# THEN RECNUM = I:I = NREC(FILE%)
      NEXT I
      IF RECNUM = 0 THEN 24025

      PRINT"Updating Record";RECNUM;
      GET #FILE%,RECNUM
      IF NEWCYCLE& < CVL(REVDATE$) THEN
         PRINT
         PRINT "Input record FAA Revision cycle date is: ";CYCLE$
         I$ = REVDATE$:GOSUB FMTREVDATE
         PRINT "Database already contains update: ";II$;" for id ";ID$
         INPUT "Do you wish to overlay with the older data (Y/N)";YUP$
         IF LEFT$(YUP$,1)="n" or LEFT$(YUP$,1)="N" THEN 24020
      ENDIF
      GOTO 24030

24025 REM Not and existing record
      PRINT"Creating ";
      RECNUM = LOF(FILE%)/RECSIZE(FILE%)
      LSET APTINFO$=MKL$(0)
      IF RECNUM = 0 THEN NREC(FILE%)=1:RECNUM = NREC(FILE%):LSET ID$=RECID$:GOTO 24030
      GET #FILE%,RECNUM:IF LEFT$(ID$,1) <> "~" THEN NREC(FILE%) = NREC(FILE%) + 1:RECNUM=NREC(FILE%)
      LSET ID$=RECID$
      LSET APTINFO$=MKL$(0)

24030 REM Parse the input Latitude/Longitude into our normal short hand notation
      GOSUB PARSECOORD:

      REM Normalize the frequency (Take 1st frequency in case of multiples)
      II$=TFREQS$+" "
      FOR I = 1 TO LEN(II$)
         II = I
         IF MID$(II$,II,1) >= "0" OR MID$(II$,II,1) <= "9" THEN 24040
      NEXT I
24040 FOR I=II TO LEN(II$)
         III = I
         IF MID$(II$,III,1) = "." THEN 24045
         IF MID$(II$,III,1) > "9" OR MID$(II$,III,1) < "0" THEN 24046
24045 NEXT I
24046 J!=VAL(MID$(II$,II,(III-II)))
      GOSUB FMTFREQ
      TWR$=I$
      IF MID$(II$,II) = " " THEN TFREQS$ = MID$(II$,III+1)

      REM Extract the Fuel Price from the FUELINFO$
      FBO$ = FUELINFO$ + " "
      WHILE LEFT$(FBO$,1) = " "
         FBO$ = MID$(FBO$,2)
      WEND

      II = 0
      FOR I = 1 TO LEN(FBO$)
         II = I
         IF MID$(FBO$,I,1) >= "0" AND MID$(FBO$,I,1) <= "9" THEN 24055
         IF MID$(FBO$,I,1) <> "." THEN 24056
24055 NEXT I
24056 IF II <= 1 THEN FBO$="":FBONAME$=FUELINFO$ ELSE FBONAME$=MID$(FBO$,II+1):FBO$=MID$(FBO$,1,II)

      REM Write the record and continue
      PRINT "Writing ";ID$;" ";NAM$
      KYA3#(RECNUM) = PKY#
      GOSUB PUTAPT

      GOTO 24020

24090 RESUME 24095
24095 CLOSE
      REM Now we must sort the file
      GOTO SORTAPT

24500 REM Export Airport information
      OUTPUT$="AIRPORTS.EXP"
      PRINT"Export to what file (ENTER=";OUTPUT$;")";:INPUT II$
      IF II$ <> "" THEN OUTPUT$=II$
      OPEN "O",#1,OUTPUT$
      GOSUB 1130
      DELIM$=CHR$(34)+","+CHR$(34)
      FUZZY$=" "
      FOR RECNUM = 1 TO NREC(FILE%)
         GET #FILE%,RECNUM
         IF ID$ = FUZZY$ THEN PRINT"Duplicate ID:";ID$,RECNUM:INPUT"Continue";ABC$
         IF ID$ < FUZZY$ THEN PRINT"Out of order ID:";ID$,RECNUM:INPUT"Continue";ABC$
         FUZZY$=ID$
         GOSUB 500
         GOSUB FMTAPT

         RECORD$=CHR$(34)+ID$+DELIM$+APT$+DELIM$+CITY$+DELIM$+STATE$
         RECORD$=RECORD$+DELIM$+LAT$+DELIM$+LONG$
         RECORD$=RECORD$+DELIM$+ELE$+DELIM$+VAR$+DELIM$

         REM We want the specified Tower/CTAF 1st in the Frequencies field
         IF RIGHT$(TWR$,1)="0" THEN TWR$=LEFT$(TWR$,LEN(TWR$)-1)
         IF RIGHT$(TWR$,2)="0 " THEN TWR$=LEFT$(TWR$,LEN(TWR$)-2)
         WHILE RIGHT$(TWR$,1) = " "
            TWR$=LEFT$(TWR$,LEN(TWR$)-1)
         WEND
         IF LEN(TWR$) > LEN(TFREQS$) THEN 24510
         IF LEFT$(TFREQS$,LEN(TWR$)) = TWR$ THEN TWR$="":GOTO 24510

         REM Could the Frequency be imbedded in the list
         I = INSTR(1,TFREQS$,TWR$)
         IF I = 0 THEN 24510

         REM Extract it and any comments with it to the front of the list
         II = INSTR(I,TFREQS$," ")
         I$=MID$(TFREQS$,1,I-1)
         J = I+LEN(TWR$)
         IF J < II THEN III$=MID$(TFREQS$,J,II-J) ELSE III$=""
         IF II > 0 THEN II$=MID$(TFREQS$,II+1) ELSE II$=""
         TFREQS$=TWR$+III$+" "+I$+II$
         TWR$=""

24510    IF TWR$ <> "" THEN RECORD$=RECORD$+TWR$
         IF TWR$ <> "" AND TFREQS$ <> "" THEN RECORD$=RECORD$+" "
         IF TFREQS$ <> "" THEN RECORD$=RECORD$+TFREQS$

         RECORD$=RECORD$+DELIM$+ATIS$+DELIM$+GROUND$+DELIM$+APPROACH$
         RECORD$=RECORD$+DELIM$+RNWY$+DELIM$+FUELINFO$+DELIM$+CYCLE$+CHR$(34)
         IF LEFT$(ID$,1) <> "~" THEN PRINT #1,RECORD$
         IF OUTPUT$ <> "SCRN:" THEN PRINT ID$;" ";APT$,CITY$;STATE$
      NEXT RECNUM
      CLOSE #1
      PRINT NREC(FILE%);"Records processed. ";
      INPUT"Continue";I

      RETURN

25000 REM Import records from NAVAIDS.INP file updating NAVAIDS.DAT
      I$="NAVAIDS.INP"
      PRINT"Import from what file (ENTER=";I$;")";:INPUT II$
      IF II$ <> "" THEN I$=II$
      ON ERROR GOTO 25010
      OPEN "I",#1,I$

      REM Read all the existing records creating keys so we can
      REM check for duplicate/updated entries as we are adding records
      GOSUB 1120
      FOR I = 1 TO NREC(FILE%)
         GET #FILE%,I
         KYA2#(I) = FNPKEY#(ID$)
      NEXT I
      GOTO 25020

25010 RESUME 25011
25011 ON ERROR GOTO 0
      PRINT "Error attempting to open '";I$;"'."
      INPUT "Press ENTER";I$
      RETURN

      REM Read an input record from NAVAIDS.INP
25020 ON ERROR GOTO 25090
      INPUT #1,RECID$,NAM$,CTY$,ST$,LAT$,LONG$,ELE$,CYCLE$,VAR$,TWR$
      ON ERROR GOTO 0
      PRINT "Processing ";LEFT$(RECID$+"        ",8);

      REM Convert the revision date into a binary we can work with
      IF CYCLE$ = "" THEN GOSUB 12000:CYCLE$ = FAACYCLE$
      I$=CYCLE$:GOSUB PACKREVDATE:NEWCYCLE& = J&

      IF LEN(RECID$)>KEYSIZE(FILE%) THEN PRINT "Skipped... not";KEYSIZE(FILE%);"or less characters":GOTO 25020

      REM See if we are to Update an existing record
      GOSUB 1120
      PKY# = FNPKEY#(LEFT$(RECID$+"     ",KEYSIZE(FILE%)))
      RECNUM = 0
      FOR I = 1 TO NREC(FILE%)
          IF KYA2#(I) = PKY# THEN RECNUM = I:I = NREC(FILE%)
      NEXT I
      IF RECNUM = 0 THEN 25025

      REM Update of an existing Navaid
      GET #FILE%,RECNUM

      IF NEWCYCLE& < CVL(REVDATE$) THEN
         PRINT
         PRINT "Input record FAA Revision cycle date is: ";CYCLE$
         I$ = REVDATE$:GOSUB FMTREVDATE
         PRINT "Database already contains update: ";II$;" for id ";ID$
         INPUT "Do you wish to overlay with the older data (Y/N)";YUP$
         IF LEFT$(YUP$,1)="n" or LEFT$(YUP$,1)="N" THEN 25020
      ENDIF

      IF RIGHT$(NAM$,3) = "VOT" OR RIGHT$(NAM$,5) = "TACAN" THEN PRINT"Replace ";APT$;" with ";NAM$;:INPUT "(Y/N)";YUP$:IF LEFT$(YUP$,1)="n" OR LEFT$(YUP$,1)="N" OR YUP$="" THEN 25020
      PRINT "Updated ";
      GOTO 25030

25025 REM See if this navaid is also a FIX
      GOSUB 1140
      I$ = RECID$
      GOSUB 400
      IF ID$=II$ THEN PRINT RECID$;" is already defined as a fix.":INPUT" Press ENTER to ignore and Continue";I$:GOTO 25020
      GOSUB 1120

      REM No, it is a new record.
      PRINT "Creating";
      REM Can we reuse a deleted record or must we append one to the end
      IF NREC(FILE%) = 0 THEN NREC(FILE%) = 1:RECNUM = NREC(FILE%):LSET ID$=RECID$:GOTO 25030
      RECNUM = NREC(FILE%)
      GET #FILE%,RECNUM
      IF LEFT$(ID$,1) <> "~" THEN NREC(FILE%) = LOF(FILE%)/RECSIZE(FILE%) + 1:RECNUM=NREC(FILE%)
      LSET ID$=RECID$

25030 REM Parse the input Latitude/Longitude into our normal short hand notation
      GOSUB PARSECOORD:

      REM Normalize the frequency
      J!=VAL(TWR$)
      GOSUB FMTFREQ
      TWR$=I$

      REM Write the record and continue
      PRINT " ";ID$;"  ";NAM$
      KYA2#(RECNUM) = PKY#
      GOSUB PUTNAV

      GOTO 25020

25090 RESUME 25095
25095 CLOSE
      REM Now we must sort the file
      GOTO SORTNAV

25500 REM Export Navaid information
      OUTPUT$="NAVAIDS.EXP"
      PRINT"Export to what file (ENTER=";OUTPUT$;")";:INPUT II$
      IF II$ <> "" THEN OUTPUT$=II$
      OPEN "O",#1,OUTPUT$
      GOSUB 1120
      FUZZY$=" "
      DELIM$=CHR$(34)+","+CHR$(34)
      FOR RECNUM = 1 TO NREC(FILE%)
         GET #FILE%,RECNUM
         GOSUB 500
         IF ID$ = FUZZY$ THEN PRINT"Duplicate ID:";ID$,RECNUM:INPUT"Continue";ABC$
         IF ID$ < FUZZY$ THEN PRINT"Out of order ID:";ID$,RECNUM:INPUT"Continue";ABC$
         FUZZY$=ID$
         GOSUB FMTNAV
         RECORD$=CHR$(34)+ID$+DELIM$+APT$+DELIM$+CITY$+DELIM$+STATE$
         RECORD$=RECORD$+DELIM$+LAT$+DELIM$+LONG$+DELIM$+ELE$+DELIM$+CYCLE$
         RECORD$=RECORD$+DELIM$+VAR$+DELIM$+TWR$+CHR$(34)
         IF LEFT$(ID$,1) <> "~" THEN PRINT #1,RECORD$
         IF OUTPUT$ <> "SCRN:" THEN PRINT ID$;" ";APT$,CITY$;" ";STATE$
      NEXT RECNUM
      CLOSE #1
      PRINT NREC(FILE%);"Records processed. ";
      INPUT"Continue";I

      RETURN

26000 REM Import records from FIXES.INP file updating FIXES.DAT
      I$="FIXES.INP"
      PRINT"Import from what file (ENTER=";I$;")";:INPUT II$
      IF II$ <> "" THEN I$=II$
      ON ERROR GOTO 26010
      OPEN "I",#1,I$

      GOTO 26020

26010 RESUME 26011
26011 ON ERROR GOTO 0
      PRINT "Error attempting to open '";I$;"'."
      INPUT "Press ENTER";I$
      RETURN

26020 REM Read an input record from FIXES.INP
      GOSUB 1140
      ON ERROR GOTO 26190
      INPUT #1,RECID$,NAM$,ST$,LAT$,LONG$,VAR$,CYCLE$,REF1$,RADIAL1$
      LINE INPUT #1,REST$
      ON ERROR GOTO 0
      PRINT "Processing ";LEFT$(RECID$+"        ",8);
      IF LEN(RECID$)<>KEYSIZE(FILE%) THEN PRINT "Skipped... not";KEYSIZE(FILE%);" characters":GOTO 26020

      REM Convert the revision date into a binary we can work with
      IF CYCLE$ = "" THEN GOSUB 12000:CYCLE$ = FAACYCLE$
      I$=CYCLE$:GOSUB PACKREVDATE:NEWCYCLE& = J&

      REM Magnetic variation may or may not be present
      IF VAR$<>"" THEN I$=VAR$:GOSUB PARSEMAGV

      DIST1$=""
      I=2

26110 IF MID$(REST$,I,1) <> CHR$(34) THEN DIST1$=DIST1$+MID$(REST$,I,1):I=I+1:GOTO 26110

      REM See if we are to Update an existing record
      I$=RECID$:WAYPT=WAYPT+1:GOSUB 100:WAYPT=WAYPT-1
      IF ID$ <> II$ THEN 26120

      IF FILE%=4 THEN RECNUM = III:PRINT "Updating ";:GOTO 26115
      REM We found the ID, but it isn't a fix.
      PRINT "Already defined as ";APT$;" in file ";fn$(FILE%)
      GOTO 26020

26115 REM Update of an existing record
      IF NEWCYCLE& < CVL(REVDATE$) THEN
         PRINT
         PRINT "Input record FAA Revision cycle date is: ";CYCLE$
         I$ = REVDATE$:GOSUB FMTREVDATE
         PRINT "Database already contains update: ";II$;" for id ";ID$
         INPUT "Do you wish to overlay with the older data (Y/N)";YUP$
         IF LEFT$(YUP$,1)="n" or LEFT$(YUP$,1)="N" THEN 26020
      ENDIF
      GOTO 26130

      REM No, it is a new record
26120 PRINT "Creating ";
      GOSUB 1140
      RECNUM = LOF(FILE%)/RECSIZE(FILE%)
      IF RECNUM = 0 THEN RECNUM = 1:LSET ID$=RECID$:GOTO 26130

      GET #FILE%,RECNUM:IF LEFT$(ID$,1) <> "~" THEN RECNUM = LOF(FILE%)/RECSIZE(FILE%) + 1
      LSET ID$=RECID$

26130 REM Parse the input Latitude/Longitude into our normal short hand notation
      GOSUB PARSECOORD:

      REM Normalize the Reference navaid
      RADIAL=VAL(RADIAL1$)
      DIST=VAL(DIST1$)

      IF VAR$<>"" AND VAR$ <> "?" THEN 26170
      REM Get the magnetic deviation from the referenced navaid record
      I$=REF1$
      GOSUB 1120:GOSUB 400
      IF ID$=II$ THEN I$=STR$(ASC(DEV$)-128):GOSUB PARSEMAGV
      GOSUB 1140

      REM Write the record and continue
26170 PRINT " ";ID$;"  ";NAM$
      GOSUB PUTFIX

      GOTO 26020

26190 RESUME 26195
26195 CLOSE
      REM Now we must sort the file
      GOTO SORTFIX

26500 REM Export navigational Fix information
      OUTPUT$="FIXES.EXP"
      PRINT"Export to what file (ENTER=";OUTPUT$;")";:INPUT II$
      IF II$ <> "" THEN OUTPUT$=II$
      OPEN "O",#1,OUTPUT$
      GOSUB 1140
      FUZZY$=" "
      DELIM$=CHR$(34)+","+CHR$(34)
      FOR RECNUM = 1 TO NREC(FILE%)
         GET #FILE%,RECNUM
         IF ID$ = FUZZY$ THEN PRINT"Duplicate ID:";ID$,RECNUM:INPUT"Continue";ABC$
         IF ID$ < FUZZY$ THEN PRINT"Out of order ID:";ID$,RECNUM:INPUT"Continue";ABC$
         FUZZY$=ID$
         GOSUB 500

         GOSUB FMTCOORD:

         I$=STR$(ASC(DEV$)-128):GOSUB PARSEMAGV:Rem format the VAR$ (Magnetic Variation)

         RECORD$=CHR$(34)+ID$+DELIM$+APT$+DELIM$+STATE$
         RECORD$=RECORD$+DELIM$+LAT$+DELIM$+LONG$+DELIM$+VAR$+DELIM$+CYCLE$
         RECORD$=RECORD$+DELIM$+REFID$+DELIM$+STR$(CVI(RADIAL$))+DELIM$+STR$(CVI(DIST$)/10)+CHR$(34)
         IF LEFT$(ID$,1) <> "~" THEN PRINT #1,RECORD$
         IF OUTPUT$ <> "SCRN:" THEN PRINT ID$;TAB(8);APT$;" ";STATE$
      NEXT RECNUM
      CLOSE #1
      PRINT NREC(FILE%);"Records processed. ";
      INPUT"Continue";I

      RETURN

40000 REM Initialize Weight and Balance defaults
      GOSUB 13100
40010 CLS:PRINT SPC(30);"Basic E6B Calculations"
      PRINT     SPC(35);"Configuration"
      PRINT
      PRINT:PRINT TAB(22);"Aircraft"
      PRINT"1 - Default file name for performance characteristics: ";PROFILE$
      PRINT"2 - Aircraft Weight and Balance calculations"
      PRINT:PRINT TAB(20);"Definitions"
      PRINT"3 - Aircraft performance characteristics"
      PRINT"4 - Aircraft Weight and Balance specification"
      PRINT"5 - Printer configuration defaults"
      PRINT:PRINT TAB(16);"Navigation Database"
      PRINT"6 - Database individual record Update"
      PRINT"7 - Database Import/Export"
      PRINT
      PRINT:INPUT"selection";I
      IF I <  1 THEN CLOSE:RUN "ALSE6B"
      IF I <= 7 THEN ON I GOSUB 14400,13700,14000,13000,14500,20100,20000
      GOTO 40010

