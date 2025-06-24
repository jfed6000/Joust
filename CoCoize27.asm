*** Joust source modifying to work on the CoCo 3
*
* To do: V 0.27 is Official Version 1.1
*
* - Final things could clean up
*     - Remove any writing to RRUC, which is currently pointing to a temp RAM byte
*     - probably a good idea to get rid of watch dog stuff
*
* V 0.27 - Added in the Rug pattern RAM test screen on bootup, an option for the user to see it in a the BASIC config.BAS program
*
* V 0.26 - Removed original sprite data to make room for a section in JOUST.BAS to modify the keys used for player controls
*        - Fixed a bug when restoring to factory settings, I wasn't changing to the CMOS page to copy the users keycodes and it was writing over code @ $D004-$D00F
*        - Changed the timing for the VSYNC & FIRQ to happen to emulate where the scanline is and it seems good at 40 & 40, no tearing of sprites and no sparkles
*          in my testing of the first 6 screens
*
* V 0.25 - Moved CoCo3 initialization code to $A100
*        - Added code to configure keyboard controls for the players, the keys can be modified from a BASIC program called MODKEYS.BAS
*
* V 0.24 - Tweaked the composite Colour palettes so that Lava is now a dark red, it was previously looking like a pinkish red
*        - Added code to reset the Daily buzzards if the user requests it from BASIC
*
* V 0.23 - Added code to exit the game and go back to BASIC if the player presses ESC/BREAK, the user can then RUN"SAVE" to save the CMOS data to disk
*        - User should now start the game from BASIC by running the program JOUST with RUN"JOUST" and select the correct options and also allows them to reset
*          High Scores and the game history settings
*
* V 0.22 - Fix audio, set a delay for the when the cliffs get destroyed so it matches the screen.  Transporter last long sample (7F is too big to fit in memory)
*          so i just left it looping the 2nd transporter sample
*        - Made Keyboard/Joystick option in FakeCMOS location of $D000 & $D001
*        - Made RGB/Composite monitor option in FakeCMOS location of $D002 & $D003
*
* V 0.21 - Fixed Transporters, players and enemies now get transported perfectly
*
* V 0.20 - Added in joystick support
*
* V 0.19 - Fixed objects going into LAVA now melt and die properly, done in routine FCLIP, CLIPER and CLPIT
*
* V 0.18 - Fixed problem with Player hitting Egg sound it was being copied from the wrong FontPalette08 instead of FontPalette09
*        - Using the last byte of block $37 = FontPalette15 for a byte to play just before we turn off sound, cleaned up liitle noise on startup
*        - Fixed issue with compiled sprites leaving trails on the screen, turned out to be a problem with my sprite compiler - All good now
*
* V 0.17 - Added Audio samples, left with 9k bytes of free space for the rest of my code/tweaks (hopefully with compression it is all good)
*        - Added code to control audio playback with looping samples
*        - Added table and data to control which sample is played at the correct time to match how joust works
*        - Audio is now working
*
* V 0.16 - Arranged loading of Blocks of memory that will flip in and out of Bank 6 ($C000-$DFFF) to make it easier to add compiled sprites
*        - Added code in IRQ2 section to use compiled sprites instead of copying data for sprites
*        - Added compiled sprites but because they don't use black around the sprites they leave trails on the screen
*        - Pretty sure the game plays 100% properly now
*
* V 0.15 - Fixed width of the Birdge on the right
*        - Fixed Lava seems screwy on start of wave sometimes, I now save the Y register in this
*          routine as the original version didn't use the Y register and it might be getting affected elsewhere
*        - Fixed RRUC now points to dummy address instead of $C900 needed to point elsewhere or be disabled
*        - Added in assembly option below if you want RGB colours or Composite colours
*        - Fixed code in routine CLIFER & LOCCLR which Erases a Cliff to make it work with the CoCo 3 screen, to make room for it I erased
*          the Copyright message after routine STENMY
*
* V 0.14 - Adding tons more BlastRectangle code, it now uses two banks of RAM = $4000 bytes
*
* V 0.13 - Added keyboard controls for player 2
*        - Fixed, now clears most of the screen just before it asks a player to enter their initials, Joust code uses the blitter to do this
*          uses RAM at $B208-
*        - Need to fix player controls when entering initials - Player 1 _.SIDE,Y = $3C Player 1 = $34 this is sent to other PIA for input
*        - Getting closer to handling initials, it will now show Z if you go left
*
* V 0.12 - Got keyboard input working, 5 inserts a coin
*        - Screen shows PRESS / SINGLE PLAY / TO START / OR / INSERT ADDITIONAL COINS FOR DUAL PLAY and properly waits there for 30 seconds then
*          goes to the attract screens showing the proper credits
*        - pressing 1 will start a 1 player game
*        - Pressing 2 will start a 2 player game
*        - Controls for player 1 working
*        - Fixed a lockup that happened when I add a coin during the drawing of the big JOUST logo demo screen, also same lockup happened
*          when I tried to insert a coin on the high score screen.  Problem was those screens could be using the graphics screens and when
*          the IRQ hits and it senses a button press like coin inserted it jumps to low ram thinking there is code at that location but it was
*          set to graphics screen mode.  Fixed it by recording and restoring the MMU blocks before any of those JSRs to low RAM in the IRQ such
*          as Coin insert, Advance button,...
*
* V 0.11 - Sped up drawing rectangles on screen to simulate the blitter erasing sprites.  Now blasting rectangles to the screen
*        - FIRQ Sample playback routine no longer uses DP memory locations, I'm using the DP to blast rectangles on the screen ASAP during the IRQ
*        - Demo screen looks good now, some flickering going on when the screen has 3 enemies flying around.  Compiled sprites should help this
*        - All three atract screens are good now. One little glitch is shown described below, compiled sprites will fix this, but note
*          to self, it can be made to look nice if ORUN2L is forced to start at $2000 (ORG  $2000) but this will change a bunch of pointers
*          and make it harder to fix any bugs...
*
* V 0.10 - Made IRQ jump directly to JOUSTs IRQ, tweaked FIRQ stuff - still not happy with drawing on screen
*
* V 0.09 - My Stack pointer wasn't being set at the beginning of my CoCo3 setup and was killing some animation data
*        - P1 Ostrich displays garbage sometimes, found it was loading data from $DFEC, can move it to start at $2000
*          but then all other pointers would be wrong and fixing troubles would be harder, so leave it for now since compiled sprites will
*          change all of this afterwards
*
* V 0.08 - Moved VideoBeam and FIRQCount to directpage RAM
*        - Removed my code called DrawLavaBridge since it's done with the software blitter now just like the real hardware
*        - Now draws players scores on the screen at routine BCDLSN
*        - Shows extra lives
*        - Erases extra lives
*
* V 0.07 - Rearranged code so that everything is now continuous from $E000-$FFFF, including my added changes at the end
*          of the original Joust code after I removed the RAM/ROM checks, etc. from Version 0.02
*          Move CoCo3 related initialaztion code and loading data to the beginning of the program listing and the end of the
*          listing is now high RAM in the $F700ish and then a section that can be used from $B206-$BCFF
*        - LAVA code working, now draws Lava at the beginning of the Demo screen,
*          need to work on bubbling lava code, (for now ignoring bubbling lava code)
*        - Adding Black text (Masking 0) which erases old text messages so we need another MMU block
*        - Demo screen is workingish
*
* V 0.06 - Added code in IRQ section to act like blitter ***
*        - Started playing with drawing the lava
*
* V 0.05 - Added FASTFONT option that speeds up loading by copying all the seperate font colours to be the same
*          to use it set FASTFONT  EQU  1,   to disable it set FASTFONT  EQU  0
*        - Working on 3rd attract screen, Demo/simulation screen at GAMSIM which jumps to SIM1A
*        - Game simulation starts with small bit of code called GAMSIM in low RAM $5EEB - shows messages on the screen
*
* V 0.04 - 2nd attract screen code, with big Joust logo working
*        - Using ./Includes/SHRAMDEF_CoCo3.SRC which had some pointers to the Joust hardware PIAs
*           - access hardware in the $C800-$C80F range - WPIAA,WCPIAA,WPIAB,WCPIAB,PIAA,CPIAA,PIAB,CPIAB
*
* V 0.03 - Modify Attract screen code
*				 - Moved clear the screen routine my own code in hi memory $F000- area Name of routine is SCCLER_TB12REV3
*				 - Moved CMOS Read A @ X to hi memroy $F000- area Name of routine is RCMOSA_TB12REV3 & also RCMOS_TB12REV3
*				 - Moved CMOS Write A @ X to hi memroy $F000- area Name of routine is WCMOSA_TB12REV3 & also WCMOS_TB12REV3
*				 - Doing High scores screen HIGHPO_TB12REV3 $414F
* 			 - Draws a border around the screen TEST $FF87
*				 - Used CoCo3 versions of CHR35,CHROUT,OUTPUT,PHR35,PHROUT,PHROT1,BCD35,BCDOUT,BCDOT1,ERTT35,OUTT35,OUT35A,ERTEXT,OUTTEXT,OUTTX2,OUTTX1
*        - Changed LDB #$E8 to LDB #$19 * extra colour inserted in the palette for the highscore border Joust $E8 = CoCo3 colour $19
*        - First attract screen is working ,showing all the high scores
*
* V 0.02 - Added some Coco specific initialization code at $B000 - $B08B (which will all be erased and used by the game once it starts up)
*				 - Added my IRQ & sound handling code in RAM @ $F774
*				 - Changed Colour values in table COLOR1 @ $E5FA
*				 - Modified Initialization code at $E000 which jumps to $E03D
*						- This sets up PIAs of Joust
*						- INITILIZE PROCESS LINKS - Link lists
*						- INITIAL COLORS
*						- INITIAL GAME TIME NBRS
*						- Set # of Credits to zero
*						- Add VPWRUP to link list to start the game code   VPWRUP = Address $5ED0 does a jump to $627F = ATTRACT  (First attract screen)
*						- Start the IRQ
*						- Start my FIRQ
*
* V 0.01 - Started with JOUST_BIG_01_Perfect_Ready_to_CoCoize.txt
*****************************************************************************************************************************************************
Add_Audio     EQU 1   ; Set to 1 to add audio sample and sample blackback code
SaveCMOS      EQU 1   ; if = 1 then we will use the CMOS settings BASIC loaded before Loading the game at $CC00 to $CFFF + Fake CMOS setting for Joystick/Keyboard and RGB/CMP moniter extra bytes
UseCompiledSprites   EQU 1  ; Set to 1 to use the compiled sprites
DefaultLengthForGodsName EQU  1 - 1 sets it to the max of 20, 3 is the default

DelayCliffDestroyerSound EQU  1 - testing to see if a delay to the cliff destroyer sound is
LotOfLives    EQU 0   ; 1 gives lots of extra men, value tweaked in CMOS settings file, 0 give usual 5 lives

Wave2EggWave  EQU  0   ; set to a 1 if you want to start the game on a testing level
DestroyBridge EQU  0

* Was 25,50 with minimal tearing when sprites at the top of the screen, no sparkles Total time is 25+50*3=175
* 50,50 seemed good maybe once saw a tear at the 1/4 down the screen position, but sparkles started occuring 50*4=200
* 40,40 seems to work well 40*4=160, weird since it should only use 25,25,25,25...
VSYNC_Count   EQU 40
FIRQ_Count    EQU 40

KeepFontData  EQU 1
TestScreenEdge EQU 0
ROMTEST   		EQU 0		; Flag to a 1 to add ROMTEST Code back in
RAMTEST   		EQU 0		; Flag to a 1 to add ROMTEST Code back in
AdvanceSwitch	EQU	0		; Flag to a 1 to add AdvanceSwitch test modes
CopyProtect		EQU	0		; Flag to a 1 to add copy protection back in (only if you want the original ROMS back otherwise game will crash) leave as 0
LavaBubble    EQU 0   ; This crashes the game, always leave it as zero, we won't have bubbly lava (It writes outside of the IRQ to graphics screen the only way to have it would slow the game down)

* Startup code will be erased when game starts - Joust erases RAM from $0000 to $B204 (Have to change it so it erases only Joust RAM)
* So Code that needs to be kept should be after $B206 and on...

* Add code in MMU block 6 - $C000 (and Block 7 - $E000 for BlastRectangle.asm)
* Each file will control where it loads, but doesn't need to set the bank to normal $3E & $3F we do that at the end of this group of files
    INCLUDE   ./Sprites/Clif5_and_Scoring.asm           * Handle drawing Cliff_5 (bottom one), also has code and fonts to draw the players scores
    INCLUDE   ./Sprites/Sprites01.asm                   * Copiled Sprites to draw Cliffs 1 to 4
    INCLUDE   ./Sprites/Sprites02.asm                   * Copiled Sprites to draw
    INCLUDE   ./Sprites/Sprites03.asm                   * Copiled Sprites to draw
    INCLUDE   ./Sprites/Sprites04.asm                   * Copiled Sprites to draw
    INCLUDE   ./Sprites/Sprites05.asm                   * Copiled Sprites to draw for Transporter objects - Enemies
    INCLUDE   ./Sprites/Sprites06.asm                   * Copiled Sprites to draw for Transporter objects - Player 1
    INCLUDE   ./Sprites/Sprites07.asm                   *** Compiled Sprites for the Enemies Mem Block shared with Audio block SampleBlock_ED

;		pragma 	nolist
  IF SaveCMOS
* Nothing to do here since BASIC should have copied the CMOS settings into RAM
  ELSE
    INCLUDE   ./Joust_CoCo3_CMOS_Data_CC00-CFFF.asm     * CMOS data located at $CC00 to $CFFF of CMOS_Storage mem block
  ENDIF                         * To start we set it as RGB = $01
* Users settings for Keyboard or Joystick  0 = Keyboard, <> 0 = Joystick
FakeCMOS_K_or_J           EQU  $D000
* Users settings for Composite or RGB monitor
FakeCMOS_RGB_or_Composite EQU  $D002

    INCLUDE   ./Sprites/BlastRectangle.asm              *
    INCLUDE   ./Joust_CoCo3_Font_Related_Code.asm       * Include Character and Font related code & message tables
;  	pragma	list

FontStart:
;		pragma 	nolist
    INCLUDE  ./Sprites/Font_Even_0.asm
;    pragma	list

* Add the other colour fonts
;		pragma 	nolist
      INCLUDE  ./Sprites/Font_Even_1toF.asm
;    pragma	list

* Setup the Banks back to normal
    ORG     MMU_Reg_Bank0_6		    * Page $C000-$FFFF  Blocks 6 & 7 (just for loading)
    FDB     $3E3F                 * Memory blocks back to normal

        opt     c,ct,cc       * show cycle count, add the counts, clear the current count

				ORG		$A100

CoCo_START:
***********************************************************
        ORCC    #$50                    * Disable interrupts
        CLR     High_Speed_Mode       	* High Speed mode enabled
        LDS     #$BF00                  * Set the stack to match Joust's Stack
        LDU     #Palette_Start          * Make all colours black so screen updates aren't visible
        LDB     #16
!
        CLR     ,U+
        DECB
        BNE     <

  IF Add_Audio
        BRA     FixSamplesLocations
* To get Loader to work with a 512k CoCo 3 we need to free up 8 blocks while loading
* since many of the audio samples are short we can load them into part of the Font blocks then once the loading is done we
* can copy them to the proper blocks and locations as the game needs
* Copy the audio samples from Font blocks to their proper blocks:  Source Bank 0 Destination Bank 3
MoveSample:
    STA   MMU_Reg_Bank0_0       * Source block
    STB   MMU_Reg_Bank0_3       * Destination block
    LDU   #$0000
!
    LDA   ,U+
    STA   ,X+
    CMPX  #$8000
    BNE   <
    RTS

FixSamplesLocations:
* 58_End_Skidding we load in block FontPalette02 then move to BLK_58_End_Skidding
    LDD   #$100*FontPalette02+BLK_58_End_Skidding
    LDX   #Sample_58_Start
    BSR   MoveSample
* 5E_Enemy_Wing_Up we load in block FontPalette03 then move to BLK_5E_Enemy_Wing_Up
    LDD   #$100*FontPalette03+BLK_5E_Enemy_Wing_Up
    LDX   #Sample_5E_Start
    BSR   MoveSample
* 5F_Enemies_Wing_Down we load in block FontPalette04 then move to BLK_5F_Enemies_Wing_Down
    LDD   #$100*FontPalette04+BLK_5F_Enemies_Wing_Down
    LDX   #Sample_5F_Start
    BSR   MoveSample
* 73_Enemy_Mounting_Bird we load in block FontPalette05 then move to BLK_73_Enemy_Mounting_Bird
    LDD   #$100*FontPalette05+BLK_73_Enemy_Mounting_Bird
    LDX   #Sample_73_Start
    BSR   MoveSample
* 75_Credit_Inserted we load in block FontPalette06 then move to BLK_75_Credit_Inserted
    LDD   #$100*FontPalette06+BLK_75_Credit_Inserted
    LDX   #Sample_75_Start
    BSR   MoveSample
* 76_CAPTURED_BY_LAVA_TROLL we load in block FontPalette07 then move to BLK_76_CAPTURED_BY_LAVA_TROLL
    LDD   #$100*FontPalette07+BLK_76_CAPTURED_BY_LAVA_TROLL
    LDX   #Sample_76_Start
    BSR   MoveSample
* 79_Cliff_Thud we load in block FontPalette08 then move to BLK_79_Cliff_Thud
    LDD   #$100*FontPalette08+BLK_79_Cliff_Thud
    LDX   #Sample_79_Start
    BSR   MoveSample
* 7C_Player_Hits_Egg we load in block FontPalette09 then move to BLK_7C_Player_Hits_Egg
    LDD   #$100*FontPalette09+BLK_7C_Player_Hits_Egg
    LDX   #Sample_7C_Start
    BSR   MoveSample
* MMU Pages back to normal
    LDD   #$383B
    STA   MMU_Reg_Bank0_0
    STB   MMU_Reg_Bank0_3
  ENDIF
***********************************************************
* Copy the messages and tables from mem block FontPalette00 to the others
* Copy from $2000 to $3FFF
        LDA     #FontPalette00                    * Memory block $28
        STA     MMU_Reg_Bank0_1			              * Page $2000-$3FFF  Block #1  - Source block
CopyMessageTablesLoop:
        INCA
        STA     MMU_Reg_Bank0_2			              * Page $4000-$5FFF  Block #2  - Destination block
        CMPA    #$38                              * Check if we are done all the mem blocks
        BEQ     DoneCopyMessageTables
        LDY     #$2000
        LDX     #$4000
!       LDU     ,Y++
        STU     ,X++
        CMPX    #FontStart-$C000+$4000
        BLE     <
        BRA     CopyMessageTablesLoop
DoneCopyMessageTables:
        LDD     #$393A                            * Normal blocks
        STD     MMU_Reg_Bank0_1			              *
***********************************************************
* CoCo Prep
        SETDP   $A0          						* Set the direct Page for the assembler
        LDA     #$A0
        TFR     A,DP

* This code masks off the two low bits written to $FF20 - we wont need this since we had to compress the audio but it is a neat feature
* So you can send the PCM Unsigned 8 Bit sample as is, no masking needed
        LDA     PIA1_Byte_1_IRQ
        PSHS    A
        ANDA    #%00110011            	* FORCE BIT2 LOW
        STA     PIA1_Byte_1_IRQ       	* $FF20 NOW DATA DIRECTION REGISTER
        LDA     #%11111100            	* OUTPUT ON DAC, INPUT ON RS-232 & CDI
        STA     PIA1_Byte_0_IRQ
        PULS    A
        STA     PIA1_Byte_1_IRQ

        LDA     #%00110100              * Setup the CoCo 3's hardware
        STA     PIA0_Byte_1_HSYNC       * HSYNC IRQ Disabled, IRQ Polarity Flag falling Edge, Data Direction Normal, Select Line LSB = 0, HSYNC Flag = 0
        STA     PIA0_Byte_3_VSYNC       * VSYNC IRQ Disabled, IRQ Polarity Flag falling Edge, Data Direction Normal, Select Line MSB = 0, VSYNC Flag = 0
        STA     PIA1_Byte_1_IRQ         * CONTROL OF CD FIRQ* TO CPU DISABLED, IRQ Polarity Falling Edge of CD, CD Flag off
        STA     PIA1_Byte_3_IRQ_Ct_Snd  * CONTROL OF Cart FIRQ* TO CPU DISABLED, IRQ Polarity Falling Edge of Cart, Cart Flag off

* Configure Audio settings
        LDA     PIA0_Byte_1_HSYNC	    	* SELECT SOUND OUT
        ANDA    #$F7                  	* RESET LSB OF MUX BIT
        STA     PIA0_Byte_1_HSYNC   		* STORE
        LDA     PIA0_Byte_3_VSYNC	    	* SELECT SOUND OUT
        ANDA    #$F7                  	* RESET MSB OF MUX BIT
        STA     PIA0_Byte_3_VSYNC     	* STORE
* Enable 6 Bit DAC output
        LDA     PIA1_Byte_3_IRQ_Ct_Snd	* GET PIA
        ORA     #%00001000              * SET 6-BIT SOUND ENABLE
        STA     PIA1_Byte_3_IRQ_Ct_Snd	* STORE

        LDA     #%01001100              * Diasble the Interrupts for now
        STA     INIT0_Register0         * CoCo 3 Mode, MMU Enabled, GIME IRQ Disabled, GIME FIRQ Disabled, Vector RAM at FEXX enabled, Standard SCS Normal, ROM Map 16k Int, 16k Ext
        LDA     #%00100000              *
        STA     INIT1_Register1         * Mem Type 64k chips, 279.365 nsec timer, MMU Task 0 - $FFA0-$FFA7
        LDA     #%10000000              *
        STA     Video_Mode_Register     * Graphics mode, Colour output, 60 hz, max vertical res

* Setup and enable the FIRQ
        LDA     #$7E                    * Write the JMP instruction if it's possbile to use Direct Page for the sample playback then use $0E = direct page JMP location, and 1 byte for address
        LDX     #FIRQ_Sound             * Enable FIRQ0 - do nothing
        STA     FIRQ_Jump_position
        STX     FIRQ_Start_Address      * FIRQ now set to playback no sound, this will be changed when an sound is played
* Setup and enable the IRQ
        LDA     #$7E                    * Write the JMP instruction
        LDX     #VSyncIRQ               *
        STA     IRQ_Jump_position       *
        STX     IRQ_Start_Address       * Point the IRQ to the VSyncIRQ address

        LDA     #%01111100              *
        STA     INIT0_Register0         * CoCo 3 Mode, MMU Enabled, GIME IRQ Enabled, GIME FIRQ Enabled, Vector RAM at FEXX enabled, Standard SCS Normal, ROM Map 16k Int, 16k Ext
        LDA     #%00001000              * $08
        STA     IRQENR                  * Enable only the Vertical Border Sync (VBORD) Interrupt
        LDD     #FIRQ_Countdown         * This is the speed the audio samples will playback at
        STD     $FF94                   * Set countdown Timer to $0254 - good for 6005.95 Hz samples
                                        * This frequency is tied to the FIRQ and scanline counter, it must be $0254 and samples must be 6005.95 Hz
        LDA     #%00100000              * $20
        STA     FIRQENR                 * Enable TIMER FIRQ Interrupt

***********************************************************
* Set Hires Screen Resolution and the number of Colours
*
* Bit Pattern   Rows Displayed
*    x00xxxxx   192
*    x01xxxxx   200
*    x10xxxxx   *zero/infinite lines on screen (undefined)
*    x11xxxxx   225
* Bit Pattern   Bytes/Row (Graphics)
*    xxx000xx   16
*    xxx001xx   20
*    xxx010xx   32
*    xxx011xx   40
*    xxx100xx   64
*    xxx101xx   80   320 4 Colours 01
*    xxx110xx   128
*    xxx111xx   160
* Bit Pattern   Colours     Pixels/Byte
*    xxxxxx00   2           8
*    xxxxxx01   4           4
*    xxxxxxl0   16          2
*    xxxxxx11   Undefined   Undefined
***********************************************************
* Most Common used settings (Uncomment the one you want to use)
*       LDA     #%00001000            	* 256 x 192 x 2 Colours  requires 6,144  bytes = $1800 RAM
*       LDA     #%00101000            	* 256 x 200 x 2 Colours  requires 6,400  bytes = $1900 RAM
*       LDA     #%01101000            	* 256 x 225 x 2 Colours  requires 7,200  bytes = $1C20 RAM
*       LDA     #%00010001            	* 256 x 192 x 4 Colours  requires 12,288 bytes = $3000 RAM
*       LDA     #%00110001            	* 256 x 200 x 4 Colours  requires 12,800 bytes = $3200 RAM
*       LDA     #%01110001            	* 256 x 225 x 4 Colours  requires 14,400 bytes = $3840 RAM
*       LDA     #%00011010            	* 256 x 192 x 16 Colours requires 24,576 bytes = $6000 RAM
*       LDA     #%00111010            	* 256 x 200 x 16 Colours requires 25,600 bytes = $6400 RAM
*       LDA     #%01111010            	* 256 x 225 x 16 Colours requires 28,800 bytes = $7080 RAM
*       LDA     #%00001100            	* 320 x 192 x 2 Colours  requires 7,680  bytes = $1E00 RAM
*       LDA     #%00101100            	* 320 x 200 x 2 Colours  requires 8,000  bytes = $1F40 RAM
*       LDA     #%01101100            	* 320 x 225 x 2 Colours  requires 10,240 bytes = $2800 RAM
*       LDA     #%00010101            	* 320 x 192 x 4 Colours  requires 15,360 bytes = $3C00 RAM
*       LDA     #%00110101            	* 320 x 200 x 4 Colours  requires 16,000 bytes = $3E80 RAM
*       LDA     #%01110101            	* 320 x 225 x 4 Colours  requires 18,000 bytes = $4650 RAM
*       LDA     #%00011110            	* 320 x 192 x 16 Colours requires 30,720 bytes = $7800 RAM
*       LDA     #%00111110            	* 320 x 200 x 16 Colours requires 32,000 bytes = $7D00 RAM
        LDA     #%01111110            	* 320 x 225 x 16 Colours requires 36,000 bytes = $8CA0 RAM
*       LDA     #%00010100            	* 640 x 192 x 2 Colours  requires 15,360 bytes = $3C00 RAM
*       LDA     #%00110100            	* 640 x 200 x 2 Colours  requires 16,000 bytes = $3E80 RAM
*       LDA     #%01110100            	* 640 x 225 x 2 Colours  requires 18,000 bytes = $4650 RAM
*       LDA     #%00011101            	* 640 x 192 x 4 Colours  requires 30,720 bytes = $7800 RAM
*       LDA     #%00111101            	* 640 x 200 x 4 Colours  requires 32,000 bytes = $7D00 RAM
*       LDA     #%01111101            	* 640 x 225 x 4 Colours  requires 36,000 bytes = $8CA0 RAM

        STA     Vid_Res_Reg

* Joust will require a screen size of 320x225
* with 16 Colours is 36,000 bytes = $8CA0 RAM
***********************************************************

* Joust seems to start drawing the blocks of the border pattern around the screen while doing the high score screen down 12 rows from the top
* Each row is 160 bytes
* 160 * 12 rows = 1920 bytes
* 1920 / 8 = 240 = $00F0

* Starting Block * $2000 / 8 = Starting point for that memory block
* Starting block * 1024 = Starting point for that memory block
* Starting block << 2 = Starting point for that memory block
* I think the Vidstart can only be in the first 512k, it won't point to memory locations higher

* Pointer value is 1920 / 8 = 240 = $00F0 in our 512k of RAM
* Point video screen veiwer to $00000 (Hi-Res page start)
        LDD     #$00F0
        STD     VidStart
        CLR     Hor_Offset_Reg        	* Don't use a Horizontal offset

* Fix byte that the audio sampling code will use just before it stops playing
        LDA     #FontPalette15                     * Memory block $37
        STA     MMU_Reg_Bank1_3                    * Set this bank as the bank that will play sound before being turned off
        STA     MMU_Reg_Bank0_6                    * Page $C000-$DFFF  Block #6
        CLR     $DFFF                              * This byte is played before sound is turned off
        LDA     #RegRAM6                           * Memory block $3E  Back to normal
        STA     MMU_Reg_Bank0_6                    * Page $C000-$DFFF  Block #6
        CLR     DoSoundLoop
        LDD     #$7FFF
        STD     GetSample+1
        STD     SampleStart                        * Make sure audio is going to be turned off when FIRQ starts

        JSR     Clear_Screen

        LDX     #FakeCMOS_K_or_J                   * See if user wants Keyboard or Joystick
        JSR     Read_CMOS_BYTES_XINTO_A            * Read 2 CMOS bytes pointed to by X in A and X=X+2
        STA     KeyboardOrJoystick                 * A has the mode -  0 = Keyboard, <> 0 = Joystick

        LDX     #FakeCMOS_RGB_or_Composite         * See if user RGB monitor colours or Composite colors
        JSR     Read_CMOS_BYTES_XINTO_A            * Read 2 CMOS bytes pointed to by X in A and X=X+2
        TSTA
        BNE     RGB_Monitor_Selected               * If A <> 0 then RGB is selected which is how it deafaults so just jump ahead
* If we get here then we must copy the composite colours over the existing RGB colours in the game
* Copy Colours for table HICOLR - 8 Colour values
    LDX   #Composite_HICOLR * Source location
    LDU   #HICOLR           * Destination location
!
    LDA    ,X+
    STA    ,U+
    CMPX   #Composite_MARCOL
    BNE     <
* Copy Colours for table MARCOL
* X will already be #Composite_MARCOL as the Source location
    LDU   #MARCOL           * Destination location
!
    LDA    ,X+
    STA    ,U+
    CMPX   #Composite_COLOR1
    BNE     <
* Copy Colours for table COLOR1
* X will already be #Composite_COLOR1 as the Source location
    LDU   #COLOR1           * Destination location
!
    LDA    ,X+
    STA    ,U+
    CMPX   #Composite_CLRTAB
    BNE     <
* Copy Colours for table CLRTAB
* X will already be #Composite_CLRTAB as the Source location
    LDU   #CLRTAB           * Destination location
!
    LDA    ,X+
    STA    ,U+
    CMPX   #Comp_PaletteSlot2_Lookup
    BNE     <
* Copy Colours for table PaletteSlot2_Lookup
* X will already be #Composite_COLOR1 as the Source location
    LDU   #PaletteSlot2_Lookup+1           * Destination location
!
    LDA    ,X+
    STA    ,U+
    CMPX   #Comp_PaletteSlot2_Lookup_End
    BNE     <

* Fix one byte used for colour value for Highscore screens border colour
    LDA   #$2D    * Compiste colour instead of RGB = $19
    STA   RGB_Monitor_Value+1

* Now Everything is initialized for the CoCo 3
RGB_Monitor_Selected:
* Set the pallete:
        LDX     #CRTAB                 * Joust game play palette
        LDU     #Palette_Start          * Destination
!
        LDD     ,X++
        STD     ,U++
        CMPX    #CRTAB+16
        BNE     <

    LDB     #CMOS_Storage
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) to CMOS Bank
* See if the user wants to see the Rug/carpet pattern - RAM test during boot up, if so then simulate it
    TST     $D010
    LBEQ    SetKeyCodes
* simulate RAM test screen

* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDA			#$04
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank

    CLR     Regular_Speed           * Slow it down so it's closer to JOUSTs speed
*
*	RAM TEST....Y = PLACE TO RETURN IF NO ERROR
*	X = SEED TO START WITH
*	A = ITERATIONS TO MAKE
;RAMTST:
;	ORCC	#$3F	NO INTERRUPTS DURING TEST
;	CLR	RWCNTL	SET TO RAM READ
  LDA   #2
  LDX   #$0000
	TFR	A,DP	COUNT AT DP.
	TFR	X,D	START WITH A PASSED SEED.
RAM2	TFR	D,U	SAVE THE SEED
RAM0	LDX	#0	MEMORY POINTER
RAM3	COMB		DONT ASK
	BITB	#9
	BNE	RAM4
	COMB
	RORA
	RORB
	BRA	RAM6
RAM4	COMB
	BITB	#9
	BNE	RAM5
	RORA
	RORB
	BRA	RAM6
RAM5	LSRA
	RORB
RAM6:
* Was a STD  ,X++
  PSHS  X
  PSHS  D
  TFR   X,D
  STA   RAM6_1+2  * Self mod below
  LDA   #160
  MUL
RAM6_1:
  ADDD  #$0000    * Self mod from above
  TFR   D,X
  PULS  D
  STA	  ,X
  STB   160,X
  PULS  X
  LEAX  2,X

	EXG	X,D	SINCE IRQ RUNS OUT OF RAM, STROKE ROVER
	TSTB
	BNE	RAM6B	EVERY 256!
;	LDB	#WDATA
;	STB	WDOG	HAVE A BONE..EVER BEEN BONED.....
	TFR	DP,B	CHECK IS NORMAL DIAGS
;	CMPB	#$FF	FF MEANS FRONT DOOR
;	BNE	RAM6C
;	LDB	PIA0	CHECK FOR ADV.
;	BITB	#2
;	BEQ	RAM6C	         NOT PRESSED.
;	JMP  DoneRAMTest 	TIME TO RETURN
RAM6C	CLRB		RETURN B TO ZERO
RAM6B	EXG	X,D	TRADE BACK
	CMPX	#$A000	DONE??
	BNE	RAM3	NOPE..CONTINUE
*
	TFR	U,D	RESTORE SEED.
	LDX	#0
RAM7	COMB
	BITB	#9
	BNE	RAM8
	COMB
	RORA
	RORB
	BRA	RAM10
RAM8	COMB
	BITB	#9
	BNE	RAM9
	RORA
	RORB
	BRA	RAM10
RAM9	LSRA
	RORB
RAM10
* was CMPD	,X++
  PSHS  X
  PSHS  D
  TFR   X,D
  STA   RAM10_1+2  * Self mod below
  LDA   #160
  MUL
RAM10_1:
  ADDD  #$0000    * Self mod from above
  TFR   D,X
  PULS  D
  CMPA  ,X
	BNE	RERROR	RAM ERROR!
  CMPB   160,X
	BNE	RERROR	RAM ERROR!
  PULS  X
  LEAX  2,X
RAM25	EXG	X,D	CHECK FOR END OF PAGE
	TSTB
	BNE	RAM17
;	LDB	#WDATA
;	STB	WDOG
	TFR	DP,B	SEE IF NORMAL RUN
;	CMPB	#$FF	FRONT DOOR??
;	BNE	RAM17C	NOPE
;	LDB	PIA0	CHECK ADV.
;	BITB	#2
;	BEQ	RAM17C
;	BRA  DoneRAMTest    	JUST RETURN (NO ERRORS)
RAM17C	CLRB
RAM17	EXG	X,D	TRADE BACK
	CMPX	#$A000	DONE??
	BNE	RAM7
	TFR	D,U	SHOVE NEW SEED OVER
	TFR	DP,A
	CMPA	#$FF	FF INDICATES FRONT DOOR
	BNE	RAM99
	TFR	U,D
	JMP	RAM0	DO ANOTHER ITERATION IN THIS TEST.
RAM99	DECA		TAKE ONE AWAY
	TFR	A,DP	RETURN
	CMPA	#$80	ZERO OUT IN AUTO CYCLE???
	BEQ	RAM99$	YEP....
	TSTA		SET FOR BELOW BRANCH
	TFR	U,D	MAKE SEED AND SAVED SEED LOOK ALIKE
	LBNE	RAM0	DO ANOTHER ITERATION
RAM99$	LDB	#1	SET BACK TO ROM
	STB	RWCNTL
	BRA  DoneRAMTest    	JUST RETURN
RERROR
  PULS  X   Restore it to Joust format
DoneRAMTest:
* Code to copy keyboard buttons from fake CMOS settings to keyboard scanning codes to allow user to select their own keys for player 1 controls and player 2 controls
* Get & Set Player 1 keycodes
SetKeyCodes:
    CLR     High_Speed_Mode       	* High Speed mode enabled
* Put graphics blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    LDA			#$3C             * A=$3C
    STA     MMU_Reg_Bank0_4  * Set Bank 4
        LDX     #CMOS_P1FColumn                   * MyOwn fake CMOS location to stare the key codes for player control keys
        LDD     ,X++
        STA     P1FColumn+1
        STB     P1FRow+1
        LDD     ,X++
        STA     P1RColumn+1
        STB     P1RRow+1
        LDD     ,X++
        STA     P1LColumn+1
        STB     P1LRow+1
* Get & Set Player 2 keycodes
        LDD     ,X++
        STA     P2FColumn+1
        STB     P2FRow+1
        LDD     ,X++
        STA     P2RColumn+1
        STB     P2RRow+1
        LDD     ,X
        STA     P2LColumn+1
        STB     P2LRow+1
    LDB     #RegRAM6
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) back to normal mode

*Check for player requested resets to game settings and Highscores
CheckHSReset:
        LDX     #SPECFN+4                             * See if user wants High scores reset
        JSR     Read_CMOS_BYTES_XINTO_A               * Read 2 CMOS bytes pointed to by X in A and X=X+2
        TSTA
        BEQ     CheckDailyBuzReset
        JSR     Clear_Screen
	      LDX	   #TODTAB_TB12REV3	TODAY'S TABLE (NORMAL BYTES)
	      LDY	   #TODAYS
	      LDB	   #ENDTOD_TB12REV3-TODTAB_TB12REV3
	      JSR	   CMSMOV
        JSR     RESHSC_TB12REV3     * RESET HIGH SCORES
        JSR     OPSET_TB12REV3      * Restore the Operator message to default
* Reset default controls
    LDB     #CMOS_Storage
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) to CMOS Bank
        LDX     #CMOS_P1FColumn
        LDD     #$F701              * P1 Flap is C
        STD     ,X++
        LDD     #$F704              * P1 Right is S
        STD     ,X++
        LDD     #$FD01              * P1 Left is A
        STD     ,X++
        LDD     #$7F20              * P2 Flap is /
        STD     ,X++
        LDD     #$EF02              * P2 Right is L
        STD     ,X++
        LDD     #$F702              * P2 Left is K
        STD     ,X
    LDB     #RegRAM6
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) back to normal mode
        CLRA
        LDX     #SPECFN+4
        JSR     Write_CMOS_A_INTO_X
        LDX     #12
HSClrDelay:
        LDY     #$0000
!
        LEAY    -1,Y
        BNE     <
        LEAX    -1,X
        BNE     HSClrDelay
CheckDailyBuzReset:
        LDX     #$CFFE                                * See if user wants Daily Buzzard scores reset
        JSR     Read_CMOS_BYTES_XINTO_A               * Read 2 CMOS bytes pointed to by X in A and X=X+2
        TSTA
        BEQ     CheckAuditsReset
	      LDX	   #TODTAB_TB12REV3	TODAY'S TABLE (NORMAL BYTES)
	      LDY	   #TODAYS
	      LDB	   #ENDTOD_TB12REV3-TODTAB_TB12REV3
	      JSR	   CMSMOV
        CLRA
        LDX     #$CFFE
        JSR     Write_CMOS_A_INTO_X
CheckAuditsReset:
        LDX     #SPECFN+2                          * See if user wants the Audits reset
        JSR     Read_CMOS_BYTES_XINTO_A            * Read 2 CMOS bytes pointed to by X in A and X=X+2
        TSTA
        BEQ     SetPlayerControls
        JSR     AUDCK4_TB12REV3
        JSR     CMINI_S_TB12REV3
        CLRA
        LDX     #SPECFN+2
        JSR     Write_CMOS_A_INTO_X
        LDX     #12
AuditsClrDelay:
        LDY     #$0000
!
        LEAY    -1,Y
        BNE     <
        LEAX    -1,X
        BNE     AuditsClrDelay

* Code to copy keyboard buttons from fake CMOS settings to keyboard scanning codes to allow user to select their own keys for player 1 controls and player 2 controls
SetPlayerControls:
    LDB     #CMOS_Storage
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) to CMOS Bank
* Get & Set Player 1 keycodes
        LDX     #CMOS_P1FColumn                   * MyOwn fake CMOS location to stare the key codes for player control keys
        LDD     ,X++
        STA     P1FColumn+1
        STB     P1FRow+1
        LDD     ,X++
        STA     P1RColumn+1
        STB     P1RRow+1
        LDD     ,X++
        STA     P1LColumn+1
        STB     P1LRow+1
* Get & Set Player 2 keycodes
        LDD     ,X++
        STA     P2FColumn+1
        STB     P2FRow+1
        LDD     ,X++
        STA     P2RColumn+1
        STB     P2RRow+1
        LDD     ,X
        STA     P2LColumn+1
        STB     P2LRow+1
    LDB     #RegRAM6
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) back to normal mode

CoCoAllSet:
        JMP     PWRUP                * Start Joust code

* Data for Composite monitor colour settings



Composite_HICOLR:
    FCB    $00
    FCB    $07
    FCB    $2C
    FCB    $18
    FCB    $2C
    FCB    $24
    FCB    $2C
    FCB    $2D
Composite_MARCOL:
    FCB    $00
    FCB    $00
    FCB    $07
    FCB    $24
    FCB    $17
    FCB    $30
    FCB    $2D
    FCB    $2D
Composite_Palette4_8:
    FCB    $00
    FCB    $00
    FCB    $07
    FCB    $17
    FCB    $07
    FCB    $17
    FCB    $07
    FCB    $00
Composite_Palette4_Other:
    FCB    $00
    FCB    $00
    FCB    $02
    FCB    $22
    FCB    $12
    FCB    $22
    FCB    $02
    FCB    $00
    FCB    $00
    FCB    $00
    FCB    $0C
    FCB    $1C
    FCB    $2C
    FCB    $1C
    FCB    $0C
    FCB    $00
    FCB    $00
    FCB    $00
    FCB    $05
    FCB    $14
    FCB    $24
    FCB    $14
    FCB    $05
    FCB    $00
    FCB    $00
    FCB    $00
    FCB    $10
    FCB    $20
    FCB    $30
    FCB    $20
    FCB    $10
    FCB    $00
Composite_COLOR1:
    FCB    $00
    FCB    $30
    FCB    $21
    FCB    $0E
    FCB    $07
    FCB    $24
    FCB    $0E
    FCB    $2D
    FCB    $15
    FCB    $0D
    FCB    $06
    FCB    $02
    FCB    $26
    FCB    $20
    FCB    $07
    FCB    $35
Composite_CLRTAB:
    FCB    $37
    FCB    $34
    FCB    $24
    FCB    $26
    FCB    $26
    FCB    $07
    FCB    $17
    FCB    $07
Comp_PaletteSlot2_Lookup:
    FCB    $1C
    FCB    $1C
    FCB    $0B
    FCB    $0B
    FCB    $19
    FCB    $19
    FCB    $18
    FCB    $18
    FCB    $1C
    FCB    $1C
    FCB    $0B
    FCB    $0B
    FCB    $19
    FCB    $19
    FCB    $18
    FCB    $18
    FCB    $0D
    FCB    $0D
    FCB    $0A
    FCB    $0A
    FCB    $1A
    FCB    $1A
    FCB    $28
    FCB    $28
    FCB    $0D
    FCB    $0D
    FCB    $0A
    FCB    $0A
    FCB    $1A
    FCB    $1A
    FCB    $28
    FCB    $28
    FCB    $1E
    FCB    $1E
    FCB    $0F
    FCB    $0F
    FCB    $20
    FCB    $20
    FCB    $37
    FCB    $37
    FCB    $1E
    FCB    $1E
    FCB    $0F
    FCB    $0F
    FCB    $20
    FCB    $20
    FCB    $37
    FCB    $37
    FCB    $1F
    FCB    $1F
    FCB    $2F
    FCB    $2F
    FCB    $31
    FCB    $31
    FCB    $3F
    FCB    $3F
    FCB    $1F
    FCB    $1F
    FCB    $2F
    FCB    $2F
    FCB    $31
    FCB    $31
    FCB    $3F
    FCB    $3F
    FCB    $2C
    FCB    $2C
    FCB    $1B
    FCB    $1B
    FCB    $2A
    FCB    $2A
    FCB    $29
    FCB    $29
    FCB    $2C
    FCB    $2C
    FCB    $1B
    FCB    $1B
    FCB    $2A
    FCB    $2A
    FCB    $29
    FCB    $29
    FCB    $1D
    FCB    $1D
    FCB    $2B
    FCB    $2B
    FCB    $3A
    FCB    $3A
    FCB    $38
    FCB    $38
    FCB    $1D
    FCB    $1D
    FCB    $2B
    FCB    $2B
    FCB    $3A
    FCB    $3A
    FCB    $38
    FCB    $38
    FCB    $2D
    FCB    $2D
    FCB    $3C
    FCB    $3C
    FCB    $3B
    FCB    $3B
    FCB    $39
    FCB    $39
    FCB    $2D
    FCB    $2D
    FCB    $3C
    FCB    $3C
    FCB    $3B
    FCB    $3B
    FCB    $39
    FCB    $39
    FCB    $2E
    FCB    $2E
    FCB    $3D
    FCB    $3D
    FCB    $3E
    FCB    $3E
    FCB    $30
    FCB    $30
    FCB    $2E
    FCB    $2E
    FCB    $3D
    FCB    $3D
    FCB    $3E
    FCB    $3E
    FCB    $30
    FCB    $30
* Joust_Colour_Match:
    FCB    $00
    FCB    $00
    FCB    $07
    FCB    $07
    FCB    $17
    FCB    $17
    FCB    $07
    FCB    $07
    FCB    $00
    FCB    $00
    FCB    $07
    FCB    $07
    FCB    $17
    FCB    $17
    FCB    $07
    FCB    $07
    FCB    $02
    FCB    $02
    FCB    $05
    FCB    $05
    FCB    $15
    FCB    $15
    FCB    $26
    FCB    $26
    FCB    $02
    FCB    $02
    FCB    $05
    FCB    $05
    FCB    $15
    FCB    $15
    FCB    $26
    FCB    $26
    FCB    $22
    FCB    $22
    FCB    $03
    FCB    $03
    FCB    $14
    FCB    $14
    FCB    $25
    FCB    $25
    FCB    $22
    FCB    $22
    FCB    $03
    FCB    $03
    FCB    $14
    FCB    $14
    FCB    $25
    FCB    $25
    FCB    $12
    FCB    $12
    FCB    $13
    FCB    $13
    FCB    $23
    FCB    $23
    FCB    $24
    FCB    $24
    FCB    $12
    FCB    $12
    FCB    $13
    FCB    $13
    FCB    $23
    FCB    $23
    FCB    $24
    FCB    $24
    FCB    $0C
    FCB    $0C
    FCB    $09
    FCB    $09
    FCB    $08
    FCB    $08
    FCB    $18
    FCB    $18
    FCB    $0C
    FCB    $0C
    FCB    $09
    FCB    $09
    FCB    $08
    FCB    $08
    FCB    $18
    FCB    $18
    FCB    $0E
    FCB    $0E
    FCB    $10
    FCB    $10
    FCB    $06
    FCB    $06
    FCB    $36
    FCB    $36
    FCB    $0E
    FCB    $0E
    FCB    $10
    FCB    $10
    FCB    $06
    FCB    $06
    FCB    $36
    FCB    $36
    FCB    $11
    FCB    $11
    FCB    $01
    FCB    $01
    FCB    $04
    FCB    $04
    FCB    $35
    FCB    $35
    FCB    $11
    FCB    $11
    FCB    $01
    FCB    $01
    FCB    $04
    FCB    $04
    FCB    $35
    FCB    $35
    FCB    $21
    FCB    $21
    FCB    $32
    FCB    $32
    FCB    $33
    FCB    $33
    FCB    $34
    FCB    $34
    FCB    $21
    FCB    $21
    FCB    $32
    FCB    $32
    FCB    $33
    FCB    $33
    FCB    $34
    FCB    $34
Comp_PaletteSlot2_Lookup_End

	IF Add_Audio
* To convert audio to CoCo 3 playback mode at 6006 hz use ffmpeg:
* ffmpeg -i source_audio.wav -acodec pcm_u8 -f u8 -ac 1 -ar 6006 -af aresample=6006:filter_size=256:cutoff=1.0 output.raw
* Batch conversion of .wav files to CoCo 3 .raw:
* for f in *.wav ; do ffmpeg -i "$f"  -acodec pcm_u8 -f u8 -ac 1 -ar 6006 -af aresample=6006:filter_size=256:cutoff=1.0  "${f%.wav}.raw" ; done
*****************************************************
* Samples that are duplicates
* 5B and DB used DB
* 69 and E9 Used E9
* 70 and F0 Used F0
* Using 6006 hz unsigned PCM audio samples
***
* Sample Name: 58_End_Skidding.raw
SampleBlock_58  EQU   BLK_58_End_Skidding     *
SampleSize_58   EQU   2670                * File size of the sample
Sample_58_Start EQU   $8000-SampleSize_58 * Starting location of sample
* Sample Name: 59_Skidding_Sound.raw
SampleBlock_59  EQU   BLK_59_Skidding_Sound   *
SampleSize_59   EQU   5527                * File size of the sample
Sample_59_Start EQU   $8000-SampleSize_59 * Starting location of sample
* Sample Name: 5B_PTERODACTYL_SCREAM.raw - using DB_PTERODACTYL_INTRODUCTION_SCREAM.raw
SampleBlock_5B  EQU   Cliff5_And_Scoring  * Memory block used to draw the platforms (other then the large one on the ground)
SampleSize_5B   EQU   4461                * File size of the sample
Sample_5B_Start EQU   $8000-SampleSize_5B * Starting location of sample
* Sample Name: 5C_Enemy_Runs.raw
SampleBlock_5C  EQU   FontPalette00       * Memory blocks for Drawing black font - we use the little space at the end of this bank for the sample below
SampleSize_5C   EQU   176                 * File size of the sample
Sample_5C_Start EQU   $8000-SampleSize_5C * Starting location of sample
* Sample Name: 5D_Enemy_Runs2.raw
SampleBlock_5D  EQU   FontPalette01       * Memory blocks for Drawing font 01 - we use the little space at the end of this bank for the sample below
SampleSize_5D   EQU   232                 * File size of the sample
Sample_5D_Start EQU   $8000-SampleSize_5D * Starting location of sample
* Sample Name: 5E_Enemy_Wing_Up.raw
SampleBlock_5E  EQU   BLK_5E_Enemy_Wing_Up    *
SampleSize_5E   EQU   766                 * File size of the sample
Sample_5E_Start EQU   $8000-SampleSize_5E * Starting location of sample
* Sample Name: 5F_Enemies_Wing_Down.raw
SampleBlock_5F  EQU   BLK_5F_Enemies_Wing_Down *
SampleSize_5F   EQU   1598                * File size of the sample
Sample_5F_Start EQU   $8000-SampleSize_5F * Starting location of sample
* Sample Name: 63_Collect_Bounty_Short.raw
SampleBlock_63  EQU   BLK_63_Collect_Bounty_Short *
SampleSize_63   EQU   7845                * File size of the sample
Sample_63_Start EQU   $8000-SampleSize_63 * Starting location of sample
* Sample Name: 64_Start_Game_Short.raw
SampleBlock_64  EQU   BLK_64_Start_Game_Short *
SampleSize_64   EQU   8119                * File size of the sample
Sample_64_Start EQU   $8000-SampleSize_64 * Starting location of sample
* Sample Name: 66_Cliff_Destroyer_Short.raw
SampleBlock_66  EQU   BLK_66_Cliff_Destroyer_Short *
SampleSize_66   EQU   8129                * File size of the sample
Sample_66_Start EQU   $8000-SampleSize_66 * Starting location of sample
* Sample Name: 69_Enemy_or_Player_Dies.raw - using E9_PTERODACTYL_DYING_SOUND.raw
SampleBlock_69  EQU   BLK_E9_PTERODACTYL_DYING_SOUND  *
SampleSize_69   EQU   4168                * File size of the sample
Sample_69_Start EQU   $8000-SampleSize_69 * Starting location of sample
* Sample Name: 70_end_of_High_Score.raw - Using F0_Part_Of_High_Score.raw
SampleBlock_70  EQU   BLK_F0_Part_Of_High_Score  * Audio Sample F0 & 70
SampleSize_70   EQU   4448                * File size of the sample
Sample_70_Start EQU   $8000-SampleSize_70 * Starting location of sample
* Sample Name: 72_Enemy_or_Player_In_Lava.raw
SampleBlock_72  EQU   CMOS_Storage        * Used in mem location $CC00 to $CFFF to match JOUST CMOS game settings
SampleSize_72   EQU   3608                * File size of the sample
Sample_72_Start EQU   $8000-SampleSize_72 * Starting location of sample
* Sample Name: 73_Enemy_Mounting_Bird.raw
SampleBlock_73  EQU   BLK_73_Enemy_Mounting_Bird *
SampleSize_73   EQU   1485                * File size of the sample
Sample_73_Start EQU   $8000-SampleSize_73 * Starting location of sample
* Sample Name: 74_Extra_Man.raw
SampleBlock_74  EQU   BLK_74_Extra_Man    *
SampleSize_74   EQU   7509                * File size of the sample
Sample_74_Start EQU   $8000-SampleSize_74 * Starting location of sample
* Sample Name: 75_Credit_Inserted.raw
SampleBlock_75  EQU   BLK_75_Credit_Inserted *
SampleSize_75   EQU   3348                * File size of the sample
Sample_75_Start EQU   $8000-SampleSize_75 * Starting location of sample
* Sample Name: 76_CAPTURED_BY_LAVA_TROLL.raw
SampleBlock_76  EQU   BLK_76_CAPTURED_BY_LAVA_TROLL *
SampleSize_76   EQU   1934                * File size of the sample
Sample_76_Start EQU   $8000-SampleSize_76 * Starting location of sample
* Sample Name: 79_Cliff_Thud.raw
SampleBlock_79  EQU   BLK_79_Cliff_Thud *
SampleSize_79   EQU   867                 * File size of the sample
Sample_79_Start EQU   $8000-SampleSize_79 * Starting location of sample
* Sample Name: 7C_Player_Hits_Egg.raw
SampleBlock_7C  EQU   BLK_7C_Player_Hits_Egg *
SampleSize_7C   EQU   1358                * File size of the sample
Sample_7C_Start EQU   $8000-SampleSize_7C * Starting location of sample
* Sample Name: 7D_Egg_Hatching.raw
SampleBlock_7D  EQU   BLK_7D_Egg_Hatching *
SampleSize_7D   EQU   6311                * File size of the sample
Sample_7D_Start EQU   $8000-SampleSize_7D * Starting location of sample
* Sample Name: DB_PTERODACTYL_INTRODUCTION_SCREAM.raw
SampleBlock_DB  EQU   Cliff5_And_Scoring  * Memory block used to draw the platforms (other then the large one on the ground)
SampleSize_DB   EQU   4461                * File size of the sample
Sample_DB_Start EQU   $8000-SampleSize_DB * Starting location of sample
* Sample Name: E5_Part_Of_High_Score.raw
SampleBlock_E5  EQU   BLK_E5_Part_Of_High_Score *
SampleSize_E5   EQU   2159                * File size of the sample
Sample_E5_Start EQU   $8000-SampleSize_E5 * Starting location of sample
* Sample Name: E7_Part_Of_High_Score.raw
SampleBlock_E7  EQU   BLK_E7_Part_Of_High_Score *
SampleSize_E7   EQU   3188                * File size of the sample
Sample_E7_Start EQU   $8000-SampleSize_E7 * Starting location of sample
* Sample Name: E9_PTERODACTYL_DYING_SOUND.raw
SampleBlock_E9  EQU   BLK_E9_PTERODACTYL_DYING_SOUND  *
SampleSize_E9   EQU   4168                * File size of the sample
Sample_E9_Start EQU   $8000-SampleSize_E9 * Starting location of sample
* Sample Name: EA_Player_Fading_In_Transporter.raw
SampleBlock_EA  EQU   BLK_EA_Player_Fading_In_Transporter *
SampleSize_EA   EQU   4468                * File size of the sample
Sample_EA_Start EQU   $8000-SampleSize_EA * Starting location of sample
* Sample Name: EB_Player_Fading_In_Transporter2.raw
SampleBlock_EB  EQU   BLK_EB_Player_Fading_In_Transporter2 *
SampleSize_EB   EQU   4468                * File size of the sample
Sample_EB_Start EQU   $8000-SampleSize_EB * Starting location of sample
* Sample Name: ED_Player_Created_In_Transporter2.raw
SampleBlock_ED  EQU   BLK_ED_Player_Created_In_Transporter2 *
SampleSize_ED   EQU   4462                * File size of the sample
Sample_ED_Start EQU   $8000-SampleSize_ED * Starting location of sample
* Sample Name: EE_Part_Of_High_Score.raw
SampleBlock_EE  EQU   BLK_EE_Part_Of_High_Score *
SampleSize_EE   EQU   4458                * File size of the sample
Sample_EE_Start EQU   $8000-SampleSize_EE * Starting location of sample
* Sample Name: EF_Part_Of_High_Score.raw
SampleBlock_EF  EQU   BLK_EF_Part_Of_High_Score *
SampleSize_EF   EQU   4412                * File size of the sample
Sample_EF_Start EQU   $8000-SampleSize_EF * Starting location of sample
* Sample Name: F0_Part_Of_High_Score.raw
SampleBlock_F0  EQU   BLK_F0_Part_Of_High_Score  * Audio Sample F0 & 70
SampleSize_F0   EQU   4448                * File size of the sample
Sample_F0_Start EQU   $8000-SampleSize_F0 * Starting location of sample
* Sample Name: F7_Enemy_Thud.raw
SampleBlock_F7  EQU   BLK_F7_Enemy_Thud *
SampleSize_F7   EQU   4461                * File size of the sample
Sample_F7_Start EQU   $8000-SampleSize_F7 * Starting location of sample
* Sample Name: F8_ENEMY_RECREATED.raw
SampleBlock_F8  EQU   BLK_F8_ENEMY_RECREATED *
SampleSize_F8   EQU   4468                * File size of the sample
Sample_F8_Start EQU   $8000-SampleSize_F8 * Starting location of sample
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     FontPalette02           **** Temp Load in Font Pallete, this will be moved to the proper block once the game starts
    ORG     $6000                   * Start of sample
    INCLUDEBIN ./Samples/58_End_Skidding.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_59
    ORG     Sample_59_Start         * Start of sample
    INCLUDEBIN ./Samples/59_Skidding_Sound.raw
*****************************************************
* Sample_5B is the same as DB so we can skip loading it here
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_5C
    ORG     Sample_5C_Start         * Start of sample
    INCLUDEBIN ./Samples/5C_Enemy_Runs.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_5D
    ORG     Sample_5D_Start         * Start of sample
    INCLUDEBIN ./Samples/5D_Enemy_Runs2.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     FontPalette03           **** Temp Load in Font Pallete, this will be moved to the proper block once the game starts
    ORG     $6000                   * Start of sample
    INCLUDEBIN ./Samples/5E_Enemy_Wing_Up.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     FontPalette04           **** Temp Load in Font Pallete, this will be moved to the proper block once the game starts
    ORG     $6000                   * Start of sample
    INCLUDEBIN ./Samples/5F_Enemies_Wing_Down.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_63
    ORG     Sample_63_Start         * Start of sample
    INCLUDEBIN ./Samples/63_Collect_Bounty_Short.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_64
    ORG     Sample_64_Start         * Start of sample
    INCLUDEBIN ./Samples/64_Start_Game_Short.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_66
    ORG     Sample_66_Start         * Start of sample
    INCLUDEBIN ./Samples/66_Cliff_Destroyer_Short.raw
*****************************************************
* Sample_69 is the same as E9 so we can skip loading it here
*****************************************************
* Sample_70 is the same as F0 so we can skip loading it here
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_72
    ORG     Sample_72_Start         * Start of sample
    INCLUDEBIN ./Samples/72_Enemy_or_Player_In_Lava.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     FontPalette05           **** Temp Load in Font Pallete, this will be moved to the proper block once the game starts
    ORG     $6000                   * Start of sample
    INCLUDEBIN ./Samples/73_Enemy_Mounting_Bird.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_74
    ORG     Sample_74_Start         * Start of sample
    INCLUDEBIN ./Samples/74_Extra_Man.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     FontPalette06           **** Temp Load in Font Pallete, this will be moved to the proper block once the game starts
    ORG     $6000                   * Start of sample
    INCLUDEBIN ./Samples/75_Credit_Inserted.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     FontPalette07           **** Temp Load in Font Pallete, this will be moved to the proper block once the game starts
    ORG     $6000                   * Start of sample
    INCLUDEBIN ./Samples/76_CAPTURED_BY_LAVA_TROLL.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     FontPalette08           **** Temp Load in Font Pallete, this will be moved to the proper block once the game starts
    ORG     $6000                   * Start of sample
    INCLUDEBIN ./Samples/79_Cliff_Thud.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     FontPalette09           **** Temp Load in Font Pallete, this will be moved to the proper block once the game starts
    ORG     $6000                   * Start of sample
    INCLUDEBIN ./Samples/7C_Player_Hits_Egg.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_7D
    ORG     Sample_7D_Start         * Start of sample
    INCLUDEBIN ./Samples/7D_Egg_Hatching.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_DB
    ORG     Sample_DB_Start         * Start of sample
    INCLUDEBIN ./Samples/DB_PTERODACTYL_INTRODUCTION_SCREAM.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_E5
    ORG     Sample_E5_Start         * Start of sample
    INCLUDEBIN ./Samples/E5_Part_Of_High_Score.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_E7
    ORG     Sample_E7_Start         * Start of sample
    INCLUDEBIN ./Samples/E7_Part_Of_High_Score.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_E9
    ORG     Sample_E9_Start         * Start of sample
    INCLUDEBIN ./Samples/E9_PTERODACTYL_DYING_SOUND.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_EA
    ORG     Sample_EA_Start         * Start of sample
    INCLUDEBIN ./Samples/EA_Player_Fading_In_Transporter.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_EB
    ORG     Sample_EB_Start         * Start of sample
    INCLUDEBIN ./Samples/EB_Player_Fading_In_Transporter2.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_ED
    ORG     Sample_ED_Start         * Start of sample
    INCLUDEBIN ./Samples/ED_Player_Created_In_Transporter2.raw        * Shared this block with Sprite Data for the Enemies on the Transporters (Sprites07.asm)
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_EE
    ORG     Sample_EE_Start         * Start of sample
    INCLUDEBIN ./Samples/EE_Part_Of_High_Score.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_EF
    ORG     Sample_EF_Start         * Start of sample
    INCLUDEBIN ./Samples/EF_Part_Of_High_Score.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_F0
    ORG     Sample_F0_Start         * Start of sample
    INCLUDEBIN ./Samples/F0_Part_Of_High_Score.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_F7
    ORG     Sample_F7_Start         * Start of sample
    INCLUDEBIN ./Samples/F7_Enemy_Thud.raw
*****************************************************
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     SampleBlock_F8
    ORG     Sample_F8_Start         * Start of sample
    INCLUDEBIN ./Samples/F8_ENEMY_RECREATED.raw
*****************************************************
* Setup memory back to normal
    ORG     MMU_Reg_Bank0_3		      * Page $6000-$7FFF  Block 3
    FCB     $3B                     * Memory block back to normal
*****************************************************
  ENDIF


* Include CoCo 3 standard hardware pointers
        INCLUDE ./CoCo3_Start.asm
        INCLUDE ./Joust_CoCo3_Memory_Description_and_Defines.asm


;*	 JOUSTI.SRC	- IMAGES
;	 MESSAGE.SRC	- MESSAGE STRINGS, FONT, MESSAGE ROUTINES
;	 TB12.SRC	- GAME UTILITIES
;	 JOUST.SRC	- THE GAME OF JOUST
;	 ATT.SRC	- THE MARQUE PAGE OF THE ATTRACT MODE
;	 SYSTEM.SRC	- BEAM INTERFERENCE/PROCESS/IRQ OVERHEAD
;	 T12.SRC	- DIAGNOSTICS & H.S.T.D. BOARDER

* All INCLUDES - MESSAGE.SRC
;	INCLUDE ./Includes/RAMDEF.SRC
	INCLUDE ./Includes/SHRAMDEF_CoCo3.SRC
	INCLUDE ./Includes/EQU.SRC
	INCLUDE ./Includes/MESSEQU.SRC
	INCLUDE ./Includes/MESSEQU2.SRC

**** JOUSTI.SRC.OUT ****

;	NOGEN
;	NCLIST
;	BLIST
*
*	COLISION OFFSET FOR DETECTING END OF POINTERS & NO COLISION POINTERS
*
COFF	EQU	$0200
; DMAFIX	EQU	$0404
*
*	POSITIVE Y-DIRECTION OFFSET MACRO
*
;POSOFF	MACRO	COLISN,XOFF,YOFF,SRC
POSOFF	MACRO																							;COLISN,XOFF,YOFF,SRC
	 FDB	\1,\2*256+256-\3,\4
	ENDM
*
*	IMAGES FOR JOUST
*
	ORG	$0000
PLAYER	EQU	0	FOR DEBUGGING PURPOSES
	FDB	CLIF1L
	FDB	CLIF1R
	FDB	CLIF2
	FDB	CLIF3U
	FDB	CLIF3L
	FDB	CLIF3R
	FDB	CLIF4
	FDB	CLIF5	  BOTTOM CLIFF
	FDB	TRANS1	TRANSPORTER #1
	FDB	TRANS2	TRANSPORTER #2
	FDB	TRANS3	TRANSPORTER #3
	FDB	TRANS4	TRANSPORTER #4
	FDB	OSTRICH
	FDB	BUZARD
	FDB	STORK	TOP AREA BIRD
	FDB	PLYR1	THE LEFT & RIGH PLAYER #1 IMAGE
	FDB	PLYR2	THE LEFT & RIGH PLAYER #2 IMAGE
	FDB	PLYR3	THE LEFT & RIGH PLAYER #3 IMAGE
	FDB	PLYR4	THE LEFT & RIGH PLAYER #4 IMAGE
	FDB	PLYR5	THE LEFT & RIGH PLAYER #5 IMAGE
	FDB	EGGI	EGG STILLS & HATCHING
	FDB	ILAVAT	LAVA TROLL HANDS
	FDB	IFLAME	LAVA FLAMES
	FDB	POOF1	PLAYER POOF DEATH
	FDB	POOF2	PLAYER POOF DEATH
	FDB	POOF3	PLAYER POOF DEATH
	FDB	IPTERO	PTERODACTYL LEFT/RIGHT 3 FRAME ANAIMATION
	FDB	COMCL5	THE COMPACTED CLIFF5
	FDB	ASH1R	A DISOULVING PTERODACTYL
	FDB	ASH1L	A DISOULVING PTERODACTYL
*
*	CLIFF1L
* 0145 START ADDRESS
* 17 BY 7 BYTES
CLIF1L	FDB	CCLF1L,CSRC1L,$0145,$1503
	FDB	       CSRC1L,$0145,$1502
CSRC1L
	IF UseCompiledSprites
    FDB Cliff_1L        * Jump address to draw compiled sprite
    FCB Sprites01       * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;    FCB $AA,$AA,$AA      * Original values of sprite
  ENDIF

  IF SaveCMOS
* If ESC/BREAK key is pressed then Exit game back to BASIC
Exit_to_BASIC:
    ORCC    #$50      * Turn off the interrupts
    STA     $FFDE     * Make sure we are in ROM mode
    STA     $FFD8     * Make sure we are at normal speed
    CLR     $71       * Make sure we do a cold reboot, so the user is confident things are back to normal
    JMP     [$FFFE]   * Jump back to the start of the BASIC ROM
  ELSE
;    FCB $AA,$AA
;    FCB $AA,$AA,$AA
;    FCB $AA,$AA,$AA
;    FCB $AA,$AA,$AA
;    FCB $AA,$AA,$A0,$88
  ENDIF
;  FCB $8C,$CC,$CA,$AE,$EC,$CC,$CC,$88,$8C,$CC,$AA,$CC,$CC,$CC,$CC,$C0
;	FCB $88,$CC,$C8,$88,$88,$EE,$CC,$88,$88,$CC,$88,$8C,$EE,$E8,$88,$00,$00
;	FCB $EE,$EE,$EE,$C8,$88,$CE,$EC,$C8,$88,$8C,$EE,$E8,$8C,$EE,$00,$00,$00
;	FCB $EE,$EE,$EE,$EE,$EE,$8E,$EE,$C8,$8C,$CE,$EE,$EE,$E8,$00,$00,$00,$00
;	FCB $EE,$EE,$EE,$EE,$EE,$EC,$EE,$EE,$88,$EE,$EE,$EE,$E0,$00,$00,$00,$00
;	FCB $00,$00,$00,$00,$00,$00,$00,$00,$8E,$EE,$EE,$E0,$00,$00,$00,$00,$00

*
CCLF1L	FDB	10+COFF,35+32+COFF	CLIF1L STARTS -9 TO 33 PIXEL (INCLUDING ZERO) Starts at $00C1
	FDB	10+COFF,35+32+COFF
	FDB	10+COFF,32+32+COFF
	FDB	10+COFF,30+32+COFF
	FDB	10+COFF,28+32+COFF
	FDB	10+COFF,27+32+COFF
	FDB	19+32+COFF,25+32+COFF
	FDB	$8100,$8100		TERMINATING POINTER
*
*	CLIFF1R
* $7E45
* 18 BY 7 BYTES
CLIF1R	FDB	CCLF1R,CSRC1R,$7E45,$1C03
	FDB	       CSRC1R,$7E45,$1C02
CSRC1R
	IF UseCompiledSprites
  FDB Cliff_1R        * Jump address to draw compiled sprite
  FCB Sprites01       * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FCB $AA,$AA,$AA      * Original values of sprite
  ENDIF
; FCB $AA
; FDB $AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAA0
; FDB $CCCC,$C888,$88CC,$888A,$CC88,$88CC,$8CCC,$CAAC,$CCAA,$AA88,$CC88,$8AA0
; FDB $000E,$EEEE,$EEE8,$8888,$CCCC,$888C,$EEE8,$8888,$88AA,$88CE,$E888,$8880
; FDB $0000,$00EE,$EEEE,$EE88,$8888,$88CE,$EEEE,$E8C8,$888C,$CEEE,$EEE8,$8880
; FDB $0000,$0000,$CCC8,$888E,$EEEE,$EEEE,$EEEE,$E8EE,$EEE8,$8EEE,$EC8E,$EEE0
; FDB $0000,$0000,$888E,$EEEE,$EEEE,$EEEE,$EEEE,$EEEE,$EEE8,$CEEE,$E8EE,$EEE0
; FDB $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0EEE,$EE88,$0000,$08EE,$EEE0
*
CCLF1R	FDB	1+COFF,36+32+COFF		CLIF1R GOES 10 MORE PIXELS OFF S
CREEN
	FDB	1+COFF,36+32+COFF
	FDB	4+COFF,36+32+COFF
	FDB	7+COFF,36+32+COFF
	FDB	9+COFF,36+32+COFF
	FDB	9+COFF,36+32+COFF
	FDB	30+COFF,36+32+COFF
	FDB	$8100,$8100		TERMINATING POINTER
*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE
*
	FCC	'JOUST (C) 1982 WILLIAMS ELECTRONICS INC.'
*
*	CLIFF2
* $2B51
* 44 BY 9 BYTES
CLIF2	FDB	CCLF2,CSRC2,$2B51,$280D
	FDB	      CSRC2,$2B51,$280C
CSRC2
	IF UseCompiledSprites
  FDB Cliff_2         * Jump address to draw compiled sprite
  FCB Sprites01       * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FCB $AA,$AA,$AA      * Original values of sprite
  ENDIF
;	FCB	 $AA,$AA,$AA,$AC,$CC,$C8,$EE,$DD
;	FCB	$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD
;	FCB	$DD,$DD,$EC,$CC,$AC,$CA,$CA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	 $AA,$AA,$AA,$AC,$CC,$CC,$CC,$CC,$C8,$8E,$ED
;	FCB	$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD
;	FCB	$DD,$DE,$88,$88,$88,$88,$CC,$CC,$CC,$CC,$CA
;	FCB	$AA,$AC,$CC,$CC,$CA,$AA,$CC,$CC,$CC,$CC,$CC
;	FCB	 $00,$0C,$CC,$C8,$88,$88,$CC,$CC,$88,$88,$EE
;	FCB	$ED,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD
;	FCB	$DE,$E8,$88,$88,$88,$88,$88,$8C,$CC,$CC,$CC
;	FCB	$8C,$C8,$8C,$CC,$CC,$88,$8E,$EE,$EE,$E0,$00
;	FCB	 $00,$00,$00,$EE,$EE,$EE,$EE,$E8,$8E,$E8,$88
;	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
;	FCB	$EE,$8E,$EE,$88,$88,$8E,$88,$88,$88,$88,$88
;	FCB	$88,$88,$88,$88,$EE,$EE,$EE,$EC,$CC,$00,$00
;	FCB	 $00,$00,$00,$00,$CC,$CC,$88,$88,$88,$EE,$E8
;	FCB	$88,$8E,$EE,$E8,$88,$8E,$88,$8E,$EE,$88,$EE
;	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$88,$CC,$8C,$C8
;	FCB	$88,$88,$EE,$EE,$88,$88,$8E,$C8,$88,$00,$00
;	FCB	 $00,$00,$00,$CC,$88,$88,$88,$EE,$EE,$88,$EE
;	FCB	$E8,$EE,$88,$8E,$EE,$E8,$88,$EE,$E8,$88,$CC
;	FCB	$EE,$EE,$EE,$EE,$EE,$88,$88,$EE,$EE,$E8,$CC
;	FCB	$88,$EE,$EE,$88,$8E,$EE,$E8,$EE,$EE,$00,$00
;	FCB	 $00,$00,$00,$88,$8E,$EE,$EE,$EE,$EE,$E8,$8E
;	FCB	$EE,$E8,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$E8,$88
;	FCB	$CE,$EE,$EE,$EE,$88,$EE,$EE,$EE,$EE,$EE,$E8
;	FCB	$CE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$00,$00,$00
;	FCB	 $00,$00,$00,$00,$00,$00,$EE,$EE,$EE,$EE,$EE
;	FCB	$EE,$88,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$E8
;	FCB	$88,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
;	FCB	$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	 $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$8E,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
;	FCB	$88,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
*
CCLF2	FDB	1+COFF,88+COFF
	FDB	1+COFF,88+COFF
	FDB	4+COFF,85+COFF
	FDB	7+COFF,84+COFF
	FDB	9+COFF,84+COFF
	FDB	7+COFF,84+COFF
	FDB	7+COFF,82+COFF
	FDB	13+COFF,67+COFF
	FDB	25+COFF,46+COFF
	FDB	$8100,$8100		TERMINATING POINTER
*
*	CLIFF3L
* 018A
* 32 BY 8 BYTES
CLIF3L	FDB	CCLF3L,CSRC3L,$018A,$240C
	FDB	       CSRC3L,$018A,$2403
CSRC3L
	IF UseCompiledSprites
  FDB Cliff_3L         * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FCB $0A,$AA,$AA      * Original values of sprite
  ENDIF
;	FCB	$AA,$AA,$AA,$EE,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD
;	FCB $DD,$DD,$DD,$DD,$DD,$EA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$06,$AA,$AA,$AA,$CC,$C8,$8E,$ED,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD
;	FCB $DD,$DD,$DD,$DD,$DE,$88,$88,$CE,$AA,$CC,$A8,$88,$88,$88,$CC,$CC
;	FCB	$08,$88,$88,$88,$8E,$E8,$88,$EE,$ED,$DD,$DD,$DD,$DD,$DD,$DD,$DD
;	FCB $DD,$DD,$DD,$DE,$E8,$88,$C8,$EE,$88,$88,$CC,$CE,$EE,$EE,$E0,$00
;	FCB	$08,$88,$88,$88,$88,$EE,$E8,$88,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
;	FCB $EE,$EE,$EE,$EE,$88,$C8,$EE,$E8,$88,$88,$8E,$EE,$EE,$EE,$00,$00
;	FCB	$08,$88,$88,$88,$88,$EE,$EE,$E8,$88,$88,$88,$88,$88,$88,$88,$88
;	FCB $88,$88,$88,$88,$EE,$EE,$EE,$88,$88,$CC,$CC,$C8,$88,$EE,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$0E,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
;	FCB $EE,$EE,$EE,$EE,$EE,$EE,$CC,$EE,$EE,$EE,$EE,$EC,$80,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$0E,$EE,$EE,$EE,$EE,$EE,$EE,$EE
;	FCB $EE,$EE,$EE,$EE,$EE,$C8,$8E,$EE,$EE,$EE,$EE,$EE,$EE,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB $00,$00,$00,$00,$08,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$00,$00,$00
*
CCLF3L	FDB	1+COFF,66+32+COFF
	FDB	1+COFF,66+32+COFF
	FDB	1+COFF,63+32+COFF
	FDB	1+COFF,62+32+COFF
	FDB	1+COFF,62+32+COFF
	FDB	15+32+COFF,59+32+COFF
	FDB	19+32+COFF,60+32+COFF
	FDB	44+32+COFF,60+32+COFF
	FDB	$8100,$8100		TERMINATING POINTER
*
*	CLIFF3U
* $6581
* 43 BY 16 BYTES
CLIF3U	FDB	CCLF3R,CSRC3U,$6581,$190F
	FDB	       CSRC3U,$6581,$190F
CSRC3U
	IF UseCompiledSprites
  FDB Cliff_3U         * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FCB $AA,$AA,$AA      * Original values of sprite
  ENDIF
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$EE,$DD,$DD,$DD,$DD
;	FCB	$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$EA,$AA,$AA,$AA
;	FCB	$CC,$C8,$88,$88,$AA,$AA,$AC,$CC,$C8,$88,$8E,$ED,$DD,$DD,$DD
;	FCB	$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DE,$8C,$AA,$8C,$C0
;	FCB	$08,$88,$8E,$EE,$EE,$CC,$CC,$CC,$CE,$88,$88,$EE,$ED,$DD,$DD
;	FCB	$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DE,$E8,$88,$8E,$EE,$00
;	FCB	$0E,$EE,$EE,$EE,$EE,$88,$8E,$EE,$E8,$EE,$88,$88,$EE,$EE,$EE
;	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$88,$8E,$EE,$E0,$00
;	FCB	$00,$0C,$CC,$CC,$88,$88,$8C,$CC,$C8,$8E,$EE,$88,$88,$88,$88
;	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$8C,$C0,$00
;	FCB	$00,$00,$EE,$EE,$EE,$EE,$EE,$EE,$EC,$8E,$EE,$EE,$EE,$EE,$E8
;	FCB	$88,$EE,$88,$88,$EE,$88,$88,$88,$EE,$EE,$8E,$E8,$00,$00
;	FCB	$00,$00,$00,$0E,$EE,$EE,$EE,$EE,$EE,$88,$8E,$EE,$EE,$E8,$8E
;	FCB	$EE,$EE,$EE,$EE,$E8,$88,$EE,$EE,$EE,$E8,$88,$E0,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$EE,$EE,$EE,$C8,$EE
;	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$88,$8E,$88,$80,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$EE,$EE,$EE,$CE,$EE
;	FCB	$EE,$EE,$EE,$EE,$EE,$88,$88,$8E,$EE,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$EE,$EE
;	FCB	$EE,$EE,$EE,$EE,$EC,$88,$EE,$EE,$E0,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$08,$EE,$EE,$EE,$00,$00,$00,$00,$00,$00
*
*	CLIFF3R
* $6581 (CONTINUED)
* 43 BY 16 BYTES
CLIF3R	FDB	CCLF3R,CSRC3R,$7F8A,$1C03
	FDB	       CSRC3R,$7F8A,$1C02
CSRC3R
	IF UseCompiledSprites
  FDB Cliff_3R         * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FCB $00,$AA,$AA      * Original values of sprite
  ENDIF
; FCB $AA
; FDB $AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAAA,$AAAA
; FDB $00CC,$C88C,$CC88,$8CAC,$CA8C,$8888,$AAAA,$AEEC,$C888,$A888,$8888,$8888
; FDB $0000,$E88E,$E888,$8EEE,$EE88,$8888,$8888,$88EE,$EEC8,$8888,$8888,$8888
; FDB $0000,$EE88,$888C,$C888,$8E88,$EEEC,$8888,$888E,$EEEE,$EEEE,$EEEE,$EEEE
; FDB $0000,$00EE,$88C8,$8EEE,$EEE8,$EEEE,$CEEE,$EE88,$8EEE,$EEEE,$EEEE,$EEEE
; FDB $0000,$000C,$CCEE,$EEEE,$EEEE,$8CEE,$E800,$0000,$00EE,$EE00,$0000,$0000
; FDB $0000,$000C,$8EEE,$EEEE,$EEEE,$E800,$0000,$0000,$0000,$0000,$0000,$0000
*
CCLF3R	FDB	1+COFF,58+COFF
	FDB	1+COFF,57+COFF
	FDB	2+COFF,56+COFF
	FDB	2+COFF,55+COFF
	FDB	4+COFF,55+COFF
	FDB	5+COFF,54+COFF
	FDB	8+COFF,53+COFF
	FDB	21+COFF,51+COFF
	FDB	21+COFF,51+COFF
	FDB	28+COFF,86+32+COFF
	FDB	41+COFF,86+32+COFF
	FDB	55+COFF,86+32+COFF
	FDB	57+COFF,86+32+COFF
	FDB	58+COFF,86+32+COFF
	FDB	61+COFF,78+COFF
	FDB	61+COFF,74+COFF
	FDB	$8100,$8100		TERMINATING POINTER
*
*	CLIFF4
* $35A3
* 32 BY 8 BYTES
CLIF4	FDB	CCLF4,CSRC4,$35A3,$240C
	FDB	      CSRC4,$35A3,$2403
CSRC4
	IF UseCompiledSprites
  FDB Cliff_4          * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FCB $AA,$AA,$AA      * Original values of sprite
  ENDIF
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$88,$CA,$AA,$AA,$AA,$CC,$CC,$CA,$AC,$CC,$CC,$AA,$AA,$CC,$CC,$AA
;	FCB $AA,$AA,$AA,$CC,$CA,$CC,$CA,$CC,$AA,$CA,$CC,$CC,$CC,$CC,$AC,$CC
;	FCB	$00,$08,$CC,$CC,$CA,$AC,$CC,$C8,$8A,$AA,$CC,$CC,$CC,$CC,$AA,$A8
;	FCB $CC,$AC,$CC,$8E,$EE,$88,$88,$CC,$C8,$88,$8C,$C8,$88,$88,$EE,$EE
;	FCB	$00,$00,$88,$CC,$C8,$88,$88,$88,$88,$88,$88,$88,$88,$8C,$CC,$CC
;	FCB $CC,$CC,$CA,$AE,$EE,$EE,$E8,$88,$88,$88,$EE,$8E,$EE,$EE,$00,$00
;	FCB	$00,$00,$08,$88,$EE,$EE,$EE,$88,$EE,$EE,$EE,$88,$EE,$CC,$88,$EE
;	FCB $88,$88,$8C,$CC,$AE,$EE,$EE,$E8,$8E,$88,$8E,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$EE,$EE,$EE,$EE,$EE,$E8,$88,$8E,$E8,$E8,$EE,$8E
;	FCB $EE,$EE,$88,$88,$CA,$EE,$EE,$EE,$EE,$EE,$88,$8E,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$0E,$EE,$EE,$EE,$EE,$E8,$EE,$EE,$EE
;	FCB $EE,$EE,$EE,$88,$88,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$EE,$EE,$EE
;	FCB $EE,$EE,$EE,$88,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
*
CCLF4	FDB	1+COFF,64+COFF
	FDB	1+COFF,64+COFF
	FDB	4+COFF,64+COFF
	FDB	5+COFF,60+COFF
	FDB	6+COFF,54+COFF
	FDB	9+COFF,56+COFF
	FDB	16+COFF,42+COFF
	FDB	26+COFF,40+COFF
	FDB	$8100,$8100		TERMINATING POINTER
*
*	CLIFF5
* $1BD3
* 93 BY 33
CLIF5	FDB	CCLF5,CSRC5,$1BD3,$5906
	FDB	      CSRC5,$1BD3,$5906
	FDB	      CSRC5L,$1BD3,$0C09
	FDB	      CSRC5R,$70D3,$0C09
CSRC5R
	IF UseCompiledSprites
  FDB CSRC5_R         * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FCB $AA,$AA,$AA      * Original values of sprite
  ENDIF
;	FCB	$AA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$CC,$88,$88,$8C,$CC,$CC,$CC,$C0
;	FCB	$88,$88,$CC,$CC,$C8,$88,$8C,$A0
;	FCB	$CC,$8C,$CC,$CC,$CE,$E8,$C8,$00
;	FCB	$EC,$8C,$CC,$EE,$E8,$EE,$00,$00
;	FCB	$8E,$E8,$EE,$88,$8E,$E0,$00,$00
;	FCB	$88,$EE,$E8,$88,$E0,$00,$00,$00
;	FCB	$88,$88,$8E,$00,$00,$00,$00,$00
;	FCB	$8E,$E8,$E0,$00,$00,$00,$00,$00
;	FCB	$88,$EE,$00,$00,$00,$00,$00,$00
;	FCB	$8E,$E0,$00,$00,$00,$00,$00,$00
;	FCB	$80,$00,$00,$00,$00,$00,$00,$00
*
CSRC5L
	IF UseCompiledSprites
  FDB CSRC5_L         * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FCB $AA,$AA,$AA      * Original values of sprite
  ENDIF
;	FCB	$AA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$CC,$CC,$CC,$88,$88,$8C,$CC,$CC
;	FCB	$0E,$EE,$EE,$88,$88,$CC,$CC,$CC
;	FCB	$0E,$EE,$EE,$EE,$88,$88,$8C,$88
;	FCB	$00,$00,$AC,$C8,$8C,$CC,$CC,$88
;	FCB	$00,$00,$EC,$CE,$EE,$E8,$EE,$88
;	FCB	$00,$00,$0E,$88,$EE,$EE,$E8,$88
;	FCB	$00,$00,$00,$E8,$88,$88,$88,$C8
;	FCB	$00,$00,$00,$0E,$EE,$EE,$E8,$88
;	FCB	$00,$00,$00,$00,$00,$00,$0E,$8E
;	FCB	$00,$00,$00,$00,$00,$00,$00,$EE
;	FCB	$00,$00,$00,$00,$00,$00,$00,$0E
;	FCB	$00,$00,$00,$00,$00,$00,$00,$0E
*
CSRC5
	IF UseCompiledSprites
  FDB CSRC5_Mid        * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FCB $AA,$AA,$AA      * Original values of sprite
  ENDIF
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$CA,$CC,$CC,$C8,$EE,$DD,$DD,$DD,$DD,$DD,$DD,$DD,$DD
;	FCB	$DD,$DD,$DD,$DD,$DD,$DD,$E8,$CC,$CC,$CA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AC,$CC,$CC
;	FCB	$CC,$88,$88,$CC,$8E,$ED,$DD,$DD,$DD,$DD,$DD,$DD,$DD
;	FCB	$DD,$DD,$DD,$DD,$DD,$DE,$88,$88,$CC,$CC,$CA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AC,$CA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
;	FCB	$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
*??????	FCB	$CC,$CC,$CC,$88,$88,$8C,$CC,$CC,$CC,$AC,$CC,$C8,$88,$8A,$AA
*??????	FCB	$A8,$88,$CC,$CC,$C8,$8A,$AA,$AA,$8C,$CC,$88,$88,$88
*??????	FCB	$88,$8C,$CC,$88,$88,$EE,$ED,$DD,$DD,$DD,$DD,$DD,$DD
*??????	FCB	$DD,$DD,$DD,$DD,$DE,$E8,$88,$88,$88,$88,$CC,$AC,$AA
*??????	FCB	$AA,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC,$88,$88,$88,$8C
*??????	FCB	$C8,$88,$CC,$CA,$AA,$AC,$CC,$CC,$C8,$88,$CC,$88,$8C
*??????	FCB	$CC,$CC,$CC,$AA,$AA,$CC,$88,$88,$8C,$CC,$CC,$CC,$C0
*??????	FCB	$0E,$EE,$EE,$88,$88,$CC,$CC,$CC,$C8,$8C,$88,$8C,$CC,$CC,$88
*??????	FCB	$CC,$C8,$88,$88,$88,$CC,$C8,$8C,$CC,$88,$88,$CC,$CC
*??????	FCB	$CC,$CC,$88,$CC,$C8,$8E,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$88,$8C,$CC,$CC,$88,$88,$8C,$C8
*??????	FCB	$8C,$CC,$88,$8C,$CC,$88,$88,$CC,$CC,$8C,$CC,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$8C,$C8,$88,$88,$88,$CC,$CC
*??????	FCB	$88,$88,$CC,$CC,$88,$88,$88,$CC,$CC,$C8,$88,$8C,$A0
*??????	FCB	$0E,$EE,$EE,$EE,$88,$88,$8C,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$CC,$8C,$CC,$CC,$CE,$E8,$C8,$00
*??????	FCB	$00,$00,$AC,$C8,$8C,$CC,$CC,$88,$8A,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$A8,$88,$8A,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$A8,$EC,$8C,$CC,$EE,$E8,$EE,$00,$00
*??????	FCB	$00,$00,$EC,$CE,$EE,$E8,$EE,$88,$80,$0E,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$08,$88,$80,$0E,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$08,$8E,$E8,$EE,$88,$8E,$E0,$00,$00
*??????	FCB	$00,$00,$0E,$88,$EE,$EE,$E8,$88,$C0,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EC,$88,$C0,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EC,$88,$EE,$E8,$88,$E0,$00,$00,$00
*??????	FCB	$00,$00,$00,$E8,$88,$88,$88,$C8,$00,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$E0,$88,$00,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$E0,$88,$88,$8E,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$0E,$EE,$EE,$E8,$88,$0E,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$88,$0E,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$8E,$E8,$E0,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$0E,$8E,$0E,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$88,$0E,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$88,$EE,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$EE,$8E,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$E8,$88,$8E,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$E8,$8E,$E0,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$0E,$80,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$E8,$88,$80,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$E8,$80,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$0E,$88,$0E,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$88,$88,$88,$0E,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$EE,$80,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$80,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$88,$88,$8C,$CC,$88,$8C,$C8
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$CC,$CC,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$CC,$CC,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$CC,$C8,$88,$88,$00,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$88,$88,$EE,$88,$8E,$88,$88
*??????	FCB	$8C,$CC,$8C,$CC,$88,$88,$CC,$CC,$88,$88,$88,$88,$CC
*??????	FCB	$8C,$CC,$88,$88,$88,$88,$88,$88,$88,$88,$EE,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$8E,$88,$88,$88,$88,$88,$8E,$EE,$CC,$CC
*??????	FCB	$C8,$88,$CC,$C8,$88,$CC,$C8,$88,$8C,$CC,$88,$8C,$CC
*??????	FCB	$CC,$CC,$C8,$C8,$00,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$08,$A8,$CC,$8E,$EE,$EC,$C8,$CC
*??????	FCB	$CC,$EE,$EC,$CC,$CC,$8E,$EE,$EE,$EE,$E8,$CC,$CC,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$EE,$E8,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$8E,$CC,$88,$88,$88,$8C,$EE,$88,$88,$8E
*??????	FCB	$EE,$8E,$EE,$8C,$CC,$CC,$88,$8C,$CC,$EE,$EE,$EE,$EE
*??????	FCB	$EC,$8C,$CC,$AA,$00,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$08,$C8,$8C,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$8E,$CC,$EE,$E8,$8E,$EE,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$8E,$EC,$C8,$88,$88,$88,$EE,$E8,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$EC,$CC,$C8,$88,$8C,$EE,$8E,$8E,$EE
*??????	FCB	$EE,$E8,$88,$E8,$C8,$8C,$CC,$C8,$8C,$EE,$EE,$E8,$8E
*??????	FCB	$E8,$E8,$CC,$AA,$80,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$08,$8C,$C8,$8E,$EE,$E8,$8E,$EE
*??????	FCB	$EE,$0C,$C8,$EE,$E8,$88,$EE,$88,$CC,$CC,$88,$8E,$EE
*??????	FCB	$8E,$EE,$88,$EE,$CC,$88,$C8,$8E,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$EC,$C8,$88,$8C,$C0,$EE,$EE,$EE,$8E
*??????	FCB	$E8,$EE,$EE,$EE,$E8,$EE,$CC,$CE,$88,$CE,$EE,$EE,$E8
*??????	FCB	$EE,$EE,$CC,$E8,$C0,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$00,$8C,$C8,$8E,$EE,$E8,$8E,$EE,$EE
*??????	FCB	$E0,$AC,$C8,$88,$88,$8E,$88,$8C,$C8,$EE,$EE,$EE,$88
*??????	FCB	$EE,$EE,$08,$E8,$CC,$CC,$EE,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$8E,$EE,$C8,$E0,$0E,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$88,$8E,$E8,$8E,$EE,$EE,$88,$80,$EE,$EE,$88
*??????	FCB	$E8,$EE,$EE,$88,$A0,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$AA,$CC,$CE,$8E,$EE,$8E,$EE,$E8,$EE
*??????	FCB	$E0,$88,$EC,$C8,$88,$E8,$8C,$CC,$EE,$E8,$8E,$88,$EE
*??????	FCB	$EE,$E0,$E8,$88,$8E,$EE,$E8,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$8E,$EE,$E8,$88,$00,$0E,$EE,$EE
*??????	FCB	$EE,$EE,$EE,$E8,$EE,$E8,$EE,$CC,$C8,$8A,$0E,$EE,$EE
*??????	FCB	$E8,$88,$8C,$CC,$A0,$00,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$8A,$88,$EE,$E8,$8E,$EE,$88,$EE,$EE
*??????	FCB	$00,$EE,$88,$88,$8E,$88,$88,$88,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$00,$0E,$C8,$88,$E8,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$E8,$88,$88,$88,$88,$8E
*??????	FCB	$EE,$EE,$EE,$EE,$EE,$88,$EE,$8C,$CC,$CA,$00,$EE,$E8
*??????	FCB	$8E,$E8,$88,$EC,$C8,$E0,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$00,$88,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$E0
*??????	FCB	$0E,$EE,$8C,$8C,$EE,$88,$8E,$EE,$EE,$EE,$EE,$EE,$E0
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$EE,$8C,$88,$88,$88,$8C
*??????	FCB	$00,$EE,$EE,$EE,$E8,$E8,$E8,$EE,$EE,$EE,$E0,$EE,$EE
*??????	FCB	$EE,$8E,$88,$8E,$EE,$80,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$CC,$8C,$88,$88,$EE,$EE,$EE,$EE,$EE,$00
*??????	FCB	$88,$88,$EE,$EE,$EE,$88,$EE,$EE,$EE,$EE,$00,$EE,$08
*??????	FCB	$88,$C8,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$8E,$E8,$88,$88,$88,$88
*??????	FCB	$C0,$EE,$E0,$0E,$EE,$EE,$8E,$EE,$EE,$EE,$E0,$0E,$EE
*??????	FCB	$EE,$EE,$EE,$88,$8C,$C0,$00,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$88,$CC,$88,$EE,$EE,$EE,$EE,$EE,$E0,$08
*??????	FCB	$88,$88,$8E,$EE,$88,$8E,$EE,$E0,$00,$00,$C8,$0E,$08
*??????	FCB	$A8,$88,$88,$8E,$E8,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$EE,$E8,$88,$88,$88
*??????	FCB	$AA,$00,$08,$80,$EE,$E8,$EE,$EE,$88,$8C,$AC,$00,$00
*??????	FCB	$0E,$EE,$EE,$EE,$8C,$CA,$E0,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$00,$00,$00,$8C,$EE,$EE,$EE,$EE,$EE,$E0,$00,$E0,$88
*??????	FCB	$88,$88,$88,$EE,$E8,$8E,$EE,$00,$00,$08,$C8,$8E,$08
*??????	FCB	$A8,$88,$88,$CE,$E8,$88,$88,$88,$88,$88,$88,$88,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$E8,$E8
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$EE,$EE,$88,$88,$8C
*??????	FCB	$CA,$E0,$08,$8C,$0E,$EE,$EE,$EE,$EE,$CC,$A8,$00,$00
*??????	FCB	$00,$0E,$EE,$EE,$EE,$CA,$A0,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$EE,$8A,$AC,$88,$88,$EE,$EE,$EE,$E0,$00,$00,$08,$A8
*??????	FCB	$88,$8C,$8E,$EE,$88,$8E,$EE,$E0,$00,$0C,$88,$80,$0C
*??????	FCB	$A8,$88,$8C,$EE,$E8,$88,$88,$88,$88,$88,$E8,$EE,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$8E,$8E,$EE
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$8E,$88,$EE,$EC,$8C,$CC
*??????	FCB	$8E,$E0,$08,$88,$C0,$00,$EE,$EE,$8E,$88,$CE,$00,$00
*??????	FCB	$00,$00,$EE,$EE,$E8,$8C,$C0,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$E8,$88,$E8,$88,$EE,$EE,$EE,$EE,$00,$00,$00,$0A,$88
*??????	FCB	$88,$C8,$EE,$E8,$88,$EE,$EE,$E0,$00,$8E,$88,$80,$0A
*??????	FCB	$CC,$88,$C8,$EE,$88,$88,$88,$88,$E8,$E8,$8E,$E8,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$88,$8E,$88,$80,$0E
*??????	FCB	$E8,$88,$88,$88,$88,$EE,$88,$88,$88,$8E,$EE,$EE,$EE
*??????	FCB	$EE,$E0,$00,$88,$CC,$00,$0E,$EE,$E8,$EE,$E8,$C0,$00
*??????	FCB	$00,$00,$0E,$EE,$EE,$E8,$88,$00,$00,$00,$00,$00,$00
*??????	FCB	$00,$00,$8E,$EE,$8E,$EE,$EE,$EE,$00,$00,$00,$00,$00,$00,$CC
*??????	FCB	$CC,$8E,$EE,$88,$8E,$EE,$EE,$00,$00,$88,$EE,$E0,$08
*??????	FCB	$CC,$CC,$8E,$E8,$88,$88,$88,$88,$88,$8E,$EE,$08,$88
*??????	FCB	$88,$88,$88,$88,$88,$88,$88,$88,$8E,$88,$88,$E0,$0E
*??????	FCB	$EE,$88,$88,$8E,$8E,$88,$EE,$EE,$88,$88,$88,$EE,$EE
*??????	FCB	$EE,$00,$00,$08,$8E,$00,$00,$EE,$EE,$E8,$88,$80,$08
*??????	FCB	$88,$00,$0E,$EE,$EE,$EE,$E8,$88,$EE,$00,$00,$00,$00
*??????	FCB	$00,$88,$CC,$88,$EE,$EE,$EE,$E0,$00,$00,$00,$C8,$E0,$00,$00
*??????	FCB	$EE,$EE,$E8,$88,$EE,$EE,$E0,$00,$00,$0E,$EE,$E0,$00
*??????	FCB	$88,$8E,$EE,$E8,$E8,$EE,$88,$8E,$EE,$EE,$00,$0E,$E8
*??????	FCB	$88,$88,$88,$88,$88,$88,$8E,$EE,$E8,$88,$EE,$E8,$00
*??????	FCB	$EE,$E8,$8E,$E8,$88,$88,$88,$88,$88,$88,$8E,$EE,$EE
*??????	FCB	$00,$00,$00,$00,$EE,$E0,$00,$0E,$E8,$8E,$88,$E0,$0E
*??????	FCB	$88,$80,$0E,$EE,$88,$8E,$E8,$88,$8E,$00,$00,$00,$00
*??????	FCB	$00,$88,$88,$EE,$EE,$EE,$EE,$E0,$00,$00,$0C,$C8,$8E,$00,$00
*??????	FCB	$EE,$E8,$88,$8E,$EE,$EE,$E0,$00,$00,$00,$EE,$00,$00
*??????	FCB	$EE,$EE,$EE,$88,$8E,$EE,$EE,$EE,$EE,$EE,$00,$00,$EE
*??????	FCB	$EE,$EE,$88,$88,$88,$88,$88,$88,$88,$8E,$EE,$EE,$00
*??????	FCB	$EE,$EE,$E8,$8E,$EE,$EE,$E8,$8E,$EE,$EE,$88,$8E,$EE
*??????	FCB	$EE,$E0,$00,$00,$00,$00,$00,$0E,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$E8,$E0,$00,$EE,$E8,$88,$8E,$EE,$8E,$00,$00,$00,$00
*??????	FCB	$00,$88,$8E,$EE,$EE,$EE,$EE,$E0,$00,$00,$00,$88,$E0,$00,$00
*??????	FCB	$0E,$EE,$EE,$EE,$EE,$EE,$E0,$00,$00,$00,$00,$00,$00
*??????	FCB	$0E,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$00,$00,$00,$0E
*??????	FCB	$EE,$EE,$EE,$8E,$E8,$88,$8E,$EE,$EE,$EE,$EE,$EE,$00
*??????	FCB	$00,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$EE,$00,$00,$00,$00,$00,$0E,$EE,$EE,$EE,$EE,$EE
*??????	FCB	$EE,$80,$00,$EE,$EE,$88,$88,$8E,$EE,$00,$00,$00,$00
*
*
CCLF5	FDB	1+COFF,186+COFF
	FDB	1+COFF,186+COFF
	FDB	1+COFF,185+COFF
	FDB	2+COFF,185+COFF
	FDB	2+COFF,184+COFF
	FDB	5+COFF,182+COFF
	FDB	5+COFF,181+COFF
	FDB	6+COFF,179+COFF
	FDB	7+COFF,176+COFF
	FDB	8+COFF,175+COFF
	FDB	14+COFF,174+COFF
	FDB	15+COFF,173+COFF
	FDB	16+COFF,171+COFF
*
	FDB	16+COFF,171+COFF	NO ONE SHOULD BE HERE OR BELOW
	FDB	16+COFF,171+COFF
	FDB	16+COFF,171+COFF
	FDB	16+COFF,171+COFF
	FDB	16+COFF,171+COFF
	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
********	FDB	16+COFF,171+COFF
	FDB	$8100,$8100		TERMINATING POINTER
*
*	COMPACTED CLIF5
*
COMCL5
;	FCB	%00001110,%10011111,%01110110,%11110111,%00101001,%11000111
;	FCB	%10110111,%11110010,%11110100,%00010101,%11011100,%01000011
;	FCB	%01010110,%01001101,%01100010,%10010111,%00101001,%11000111
;	FCB	%00110111,%11011000,%10111101,%00001010,%00011010,%01010000
;	FCB	%10110101,%11000100,%10001010,%11100100,%10011011,%10110110
;	FCB	%10101100,%01011001,%10101001,%01111010,%10000101,%11011110
;	FCB	%01010110,%10010110,%01010110,%10110001,%01011110,%00110001
;	FCB	%10010011,%10010110,%01010010,%11101111,%10101100,%11000100
;	FCB	%10101001,%00100101,%00101010,%10010101,%10101100,%11001000
;	FCB	%10101010,%01010010,%10101001,%00100110,%10110011,%01001010
;	FCB	%11100100,%10101011,%00011100,%00111111,%01100010,%01001101
;	FCB	%01000011,%11010101,%00101111,%01010000,%10101101,%00100100
;	FCB	%10101101,%01000010,%10110101,%10001001,%01010101,%00001010
;	FCB	%11010100,%00100011,%01111101,%01001011,%11010111,%00101001
;	FCB	%01010000,%10101101,%01010010,%10110101,%10001011,%01011100
;	FCB	%10101101,%00010011,%00101001,%01001001,%00101101,%01011000
;	FCB	%10110101,%00100000,%10111101,%01100011,%11011101,%11000111
;	FCB	%00000100,%11110111,%00111101,%00000010,%01111000,%10100101
;	FCB	%11001001,%00010101,%00111110,%01111011,%10011000,%10110000
;	FCB	%11011010,%01010100,%00101111,%01010100,%11101100,%00010010
;	FCB	%10000110,%11011000,%11101100,%00010010,%10000110,%11110011
;	FCB	%11111110,%11100101,%01101010,%11111100,%10100111,%10001011
;	FCB	%00001111,%10100101,%01101111,%10010100,%11101010,%01010000
;	FCB	%00000010,%01001111,%11000011,%00010100,%00000000,%10010011
;	FCB	%11110000,%10000101,%00111110,%01010011,%10101001,%01001111
;	FCB	%00010111,%00011111,%01000010,%11111101,%01001111,%01110000
;	FCB	%00001001,%01111111,%10101000,%01111011,%10000000,%01001011
;	FCB	%11111101,%01000010,%10111101,%01001111,%11100010,%01000000
;	FCB	%11111001,%00100111,%10111001,%01000000,%00001001,%01111111
;	FCB	%00001000,%01010000,%00000010,%01011111,%11000011,%10011111
;	FCB	%11000100,%10010000,%01000111,%01010011,%10000000,%01001101
;	FCB	%11101000,%01110000,%00001001,%10111111,%00101001,%11110011
;	FCB	%11111000,%10011110,%00111111,%10011111,%11100000,%00010011
;	FCB	%01111010,%00011100,%00000010,%01101111,%01000010,%10011110
;	FCB	%00100010,%00000001,%00111110,%01000001,%00110011,%10110001
;	FCB	%00000100,%11001110,%10000101,%00111100,%01000100,%01000111
;	FCB	%11110011,%10000000,%01001011,%11101100,%01110000,%00001001
;	FCB	%01111101,%00001100,%01000100,%01000111,%11010000,%11100000
;	FCB	%00010010,%01111001,%00000111,%00000000,%10010011,%11110011
;	FCB	%00010001,%00100000,%00000100,%11011001,%10001000,%10010000
;	FCB	%01110010,%10110101,%01001010,%01010001,%11110010,%11010100
;	FCB	%00010011,%00001011,%01010001,%01100010,%10110101,%11001100
;	FCB	%01000100,%10000011,%00010100,%11101010,%01111110,%11100101
;	FCB	%01101110,%01010110,%10110001,%01101010,%01010001,%01001011
;	FCB	%10010101,%10100010,%01000101,%00111000,%01001110,%01111110
;	FCB	%01101001,%01011110,%11110101,%01001010,%11010101,%00101011
;	FCB	%01011000,%10101101,%01010010,%01010101,%11001111,%01110011
;	FCB	%00010001,%00010001,%10011101,%11100101,%00101110,%01011011
;	FCB	%10100101,%11001011,%01010101,%11101111,%01110010,%01010111
;	FCB	%11001011,%01010001,%01100010,%10111100,%00101000,%00111111
;	FCB	%01001010,%01001001,%11101010,%01110111,%00101011,%11110010
;	FCB	%10111111,%00101111,%01010100,%10101101,%00101111,%11110111
;	FCB	%00101011,%01010001,%11000100,%01000100,%01100111,%10101000
;	FCB	%01111010,%00100010,%01111110,%10010101,%01111010,%00010101
;	FCB	%11100010,%00100101,%00111010,%01010010,%01001010,%11110000
;	FCB	%10101100,%11111101,%10101011,%00011110,%10100111,%11001111
;	FCB	%11110010,%01000111,%01010011,%11111100,%11110101,%00001011
;	FCB	%01010100,%00111101,%01111110,%10000101,%00111110,%01111111
;	FCB	%10010100,%10101000,%11110011,%00010001,%00010000,%10000101
;	FCB	%00101010,%00010110,%11101000,%01011111,%11100001,%00101110
;	FCB	%01010111,%10101001,%01001110,%10000101,%10101010,%10010101
;	FCB	%11111001,%01011110,%10000101,%00111010,%01010100,%00111101
;	FCB	%01000011,%11110000,%10111000,%11111101,%00101011,%00010100
;	FCB	%10111000,%00100011,%11100101,%00111110,%01001001,%11111001
;	FCB	%01001110,%10110111,%11101000,%01111010,%01000111,%11001011
;	FCB	%01110100,%10111111,%11001111,%01100010,%00100000,%00110010
;	FCB	%10010101,%00001011,%01110100,%00100100,%01111100,%01101101
;	FCB	%00101001,%00000111,%11101010,%01010010,%11100100,%10001110
;	FCB	%10000101,%10111110,%00110011,%11111100,%10110101,%01001110
;	FCB	%00011000,%10010101,%11111101,%11001111,%11010000,%00011111
;	FCB	%11010100,%10100111,%01000010,%11111101,%01001110,%00011011
;	FCB	%10100001,%11111110,%01011011,%10100001,%11011100,%01001110
;	FCB	%00001000,%11010110,%11111111,%00101011,%11110010,%11011111
;	FCB	%00101011,%11110000,%10000111,%11101001,%01010100,%11111101
;	FCB	%00001010,%11010101,%11101000,%01111110,%10000101,%11111110
;	FCB	%00111110,%11000101,%10111000,%01101000,%01011011,%10101001
;	FCB	%01010000,%01110111,%11001010,%11111100,%10100111,%01011010
;	FCB	%10000111,%01111000,%00100011,%10110001,%01011011,%10111000
;	FCB	%10011100,%00110011,%10110100,%00101011,%11010000,%10101111
;	FCB	%01000010,%11011101,%00000010,%01110111,%00111111,%00100000
;	FCB	%10011001,%11010100,%01111111,%10101010,%01111110,%00011101
;	FCB	%10011111,%10011000,%01001101,%11101000,%01010011,%11100101
;	FCB	%10101110,%11010000,%00101111,%01000010,%10011101,%01001111
;	FCB	%11010010,%11100111,%11110001,%00111000,%00100001,%00010001
;	FCB	%11101000,%00010111,%11100111,%10111001,%11101010,%01110101
;	FCB	%00100111,%01111100,%00000010,%00100001,%01001111,%10011110
;	FCB	%10010010,%01111010,%10000000,%10011111,%10011111,%11100111
;	FCB	%11111001,%00100111,%11100000,%10001111,%10011111,%10101001
;	FCB	%01011111,%10011000,%10011000,%00010010,%11100111,%10101100
;	FCB	%01001100,%11101000,%00011000,%10010001,%11010000,%10010101
;	FCB	%11010000,%00100111,%11000010,%10011110,%10000010,%00010001
;	FCB	%01001110,%01011001,%11101110,%00010111,%10100000,%01111111
;	FCB	%10010010,%10111010,%00000010,%11111010,%10010100,%10110001
;	FCB	%00110000,%00100001,%01001010,%10000100,%11011110,%10000000
;	FCB	%10000010,%10111101,%01001011,%01110111,%00011101,%11001110
;	FCB	%00111111,%10001100,%11101100,%10000010,%10011100,%00111101
;	FCB	%00101011,%11001001,%00101000,%11010100,%00100001,%11000010
;	FCB	%11111100,%10110111,%01010011,%11011101,%11110101,%11000001
;	FCB	%00111111,%00101001,%01110111,%11111000,%10011000,%00110011
;	FCB	%11010011,%01111010,%10001111,%11100000,%10100010,%10111101
;	FCB	%00001010,%11110111,%00011001,%11101010,%00011111,%11100011
;	FCB	%00111011,%01110011,%11010100,%11100001,%01001001,%11111110
;	FCB	%01111110,%00100110,%01011011,%10111001,%01001011,%10111111
;	FCB	%10100000,%01000011,%11011100,%00010111,%11010010,%11101111
;	FCB	%00100100,%10000010,%01111111,%01010001,%11000101,%10000010
;	FCB	%01111100,%10100011,%11101011,%00010010,%01111001,%00000011
;	FCB	%00111011,%01100011,%11011100,%10101111,%01010010,%11011101
;	FCB	%10000111,%01010100,%10100000,%11101110,%11011000,%11110101
;	FCB	%01111001,%10100111,%11111001,%01001110,%00110010,%01111111
;	FCB	%10010101,%11100010,%00100111,%11101000,%01010111,%11110111
;	FCB	%00101011,%01110010,%10011101,%00000010,%10011110,%10101000
;	FCB	%01101111,%10011111,%10100001,%11101111,%11001010,%00001111
;	FCB	%11010000,%10100101,%10001011,%00001111,%10101001,%11111010
;	FCB	%10010010,%10111001,%00100011,%01101100,%01111011,%10010101
;	FCB	%11101010,%01011111,%10101000,%11001111,%11010100,%10100000
;	FCB	%11011010,%01010100,%00111101,%11001010,%01110010,%10001111
;	FCB	%11110011,%11110100,%00101001,%11000110,%00001111,%11010100
;	FCB	%10100000,%01001110,%01011001,%01001110,%01001001,%00110011
;	FCB	%10101000,%01000010,%10010101,%01000011,%01111100,%10101111
;	FCB	%11001111,%01001010,%00000100,%01110101,%00110001,%01100001
;	FCB	%10010101,%11111001,%00100111,%10011100,%00011010,%11100101
;	FCB	%01111010,%10010111,%11101100,%00010000,%10101111,%01000001
;	FCB	%10010110,%10111001,%01001110,%01110001,%01011111,%10000001
;	FCB	%01100011,%11110110,%00111111,%01000000,%10111101,%11001111
;	FCB	%11110011,%11110100,%00101101,%11001000,%00100100,%01110111
;	FCB	%00001000,%01111110,%11000001,%11111011,%00010100,%00001010
;	FCB	%01010100,%00010101,%11010100,%10100111,%10001010,%00000100
;	FCB	%00101001,%01010000,%10010011,%11001001,%00011101,%11001111
;	FCB	%11011100,%00111111,%01010010,%11111100,%10000000,%11011101
;	FCB	%01000010,%10010110,%11111001,%11111110,%01010011,%10101001
;	FCB	%01111110,%10100001,%00111000,%10000001,%01101110,%10100101
;	FCB	%01111110,%01010000,%00101111,%01000010,%10011100,%01000000
;	FCB	%10111111,%00101000,%00101111,%01100000,%10011101,%00001111
;	FCB	%11010000,%11111101,%00000111,%11010100,%10100000,%01011110
;	FCB	%10100101,%00111011,%00011111,%11000101,%00000011,%00010010
;	FCB	%11111001,%00000001,%00101010,%00011111,%10110000,%01011110
;	FCB	%11000100,%10001110,%01001000,%01001110,%11000000,%10001110
;	FCB	%10100100,%11011110,%11000000,%10001110,%00100010,%01011111
;	FCB	%10100000,%01111110,%10000100,%10001110,%10000101,%11111010
;	FCB	%10010010,%00111001,%11000000,%11101111,%10011111,%10101000
;	FCB	%01011110,%11000101,%01111110,%01111111,%00010100,%00001010
;	FCB	%01001100,%11100100,%10000100,%00111111,%00100000,%00011101
;	FCB	%11000100,%00000000,%10011111,%00100100,%00010011,%11110010
;	FCB	%10011101,%10001001,%10111101,%10000000,%11110111,%00110100
;	FCB	%00011111,%11110010,%10100001,%10111011,%10010101,%11110001
;	FCB	%10000000
CL5LEN	EQU	*-COMCL5
*
*
*	TRANSPORTERS
TRASRC
	IF UseCompiledSprites
  FDB TRANSPORTERS        * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
	FDB	$DDDD          * Original values of sprite
;  FCB $DD
  ENDIF
;  FCB $DD
;  FDB $DDDD,$DDDD,$DDDD,$DDDD,$DDDD
;	FDB	$0DDD,$DDDD,$DDDD,$DDDD,$DDDD,$DDDD,$DDD0
;	FDB	$000D,$DDDD,$DDDD,$DDDD,$DDDD,$DDDD,$D000
*
TRANS1	FDB	$0A00,TRASRC,$3551,$0A07
TRANS2	FDB	$0A00,TRASRC,$7081,$0A07
TRANS3	FDB	$0A00,TRASRC,$088A,$0A07
TRANS4	FDB	$0A00,TRASRC,$3CD3,$0A07
*
*
*
*	OSTRICH FLYING, RUNNING, SKIDDING, & STANDING FRAMES
*
*
OSTRICH	EQU	*
ORSKID	FDB	CSKIDR,$00EF,ORUNSR
	FDB	CSKIDL,$01EF,ORUNSL
ORSTND	FDB	CSTN4R,$00ED,ORUN4R	CSTN0R,$00ED,ORUN0R
	FDB	CSTN4L,$01ED,ORUN4L	CSTN0L,$00ED,ORUN0L
ORRUN1	FDB	CSTN1R,$00ED,ORUN1R
	FDB	CSTN1L,$01ED,ORUN1L
ORRUN2	FDB	CSTN2R,$00ED,ORUN2R
	FDB	CSTN2L,$02ED,ORUN2L
ORRUN3	FDB	CSTN3R,$00ED,ORUN3R
	FDB	CSTN3L,$02ED,ORUN3L
ORRUN4	FDB	CSTN4R,$00ED,ORUN4R
	FDB	CSTN4L,$01ED,ORUN4L
ORFLAP	FDB	CWNG1R,$00ED,OFLY1R
	FDB	CWNG1L,$00ED,OFLY1L
ORFLOP	FDB	CWNG2R,$00ED,OFLY1R	,OFLY2R
	FDB	CWNG2L,$00ED,OFLY1L	,OFLY2L
ORFLIP	FDB	CWNG3R,$00ED,OFLY3R
	FDB	CWNG3L,$00ED,OFLY3L
*
*	STORK
*
STORK	EQU	*
	FDB	CSKIDR,$00EE,SRUNSR
	FDB	CSKIDL,$02EE,SRUNSL
	FDB	CSTN4R,$00ED,SRUN4R	CSTN0R,$00ED,SRUN0R
	FDB	CSTN4L,$01ED,SRUN4L	CSTN0L,$00ED,SRUN0L
	FDB	CSTN1R,$00ED,SRUN1R
	FDB	CSTN1L,$00ED,SRUN1L
	FDB	CSTN2R,$00ED,SRUN2R
	FDB	CSTN2L,$01ED,SRUN2L
	FDB	CSTN3R,$00ED,SRUN3R
	FDB	CSTN3L,$01ED,SRUN3L
	FDB	CSTN4R,$00ED,SRUN4R
	FDB	CSTN4L,$01ED,SRUN4L
	FDB	CWNG1R,$00ED,SFLY1R
	FDB	CWNG1L,$01ED,SFLY1L
	FDB	CWNG2R,$00ED,SFLY1R	,SFLY2R
	FDB	CWNG2L,$01ED,SFLY1L	,SFLY2L
	FDB	CWNG3R,$00ED,SFLY3R
	FDB	CWNG3L,$00ED,SFLY3L
*
CWNG1R
CWNG2R
CWNG3R	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,8+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,17+COFF
	FDB	2+COFF,10+COFF
	FDB	2+COFF,14+COFF
	FDB	2+COFF,14+COFF
	FDB	4+COFF,13+COFF
	FDB	5+COFF,12+COFF
	FDB	5+COFF,12+COFF
	FDB	5+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CWNG1L
CWNG2L
CWNG3L	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	2+COFF,13+COFF
	FDB	9+COFF,17+COFF
	FDB	5+COFF,17+COFF
	FDB	5+COFF,17+COFF
	FDB	6+COFF,15+COFF
	FDB	7+COFF,14+COFF
	FDB	7+COFF,14+COFF
	FDB	7+COFF,14+COFF
	FDB	7+COFF,8+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CSKIDR	FDB	$8000,$8000	NO COLISION ON THIS LINE
	FDB	$8000,$8000	NO COLISION ON THIS LINE
	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,8+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,10+COFF
	FDB	2+COFF,10+COFF
	FDB	2+COFF,11+COFF
	FDB	3+COFF,11+COFF
	FDB	4+COFF,11+COFF
	FDB	5+COFF,11+COFF
	FDB	5+COFF,11+COFF
	FDB	6+COFF,10+COFF
	FDB	7+COFF,11+COFF
	FDB	7+COFF,11+COFF
	FDB	7+COFF,12+COFF
	FDB	7+COFF,13+COFF
	FDB	7+COFF,14+COFF
	FDB	8+COFF,15+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CSTN1R	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,8+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,17+COFF
	FDB	2+COFF,10+COFF
	FDB	2+COFF,14+COFF
	FDB	3+COFF,14+COFF
	FDB	4+COFF,13+COFF
	FDB	5+COFF,12+COFF
	FDB	5+COFF,12+COFF
	FDB	6+COFF,10+COFF
	FDB	4+COFF,10+COFF
	FDB	4+COFF,9+COFF
	FDB	6+COFF,9+COFF
	FDB	7+COFF,10+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,10+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE
*
	FCC	'JOUST (C) 1982 WILLIAMS ELECTRONICS INC.'
*
CSTN2R	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,8+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,17+COFF
	FDB	2+COFF,10+COFF
	FDB	2+COFF,14+COFF
	FDB	3+COFF,14+COFF
	FDB	4+COFF,13+COFF
	FDB	5+COFF,12+COFF
	FDB	5+COFF,12+COFF
	FDB	6+COFF,10+COFF
	FDB	8+COFF,10+COFF
	FDB	7+COFF,10+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,12+COFF
	FDB	6+COFF,12+COFF
	FDB	6+COFF,13+COFF
	FDB	6+COFF,13+COFF
	FDB	6+COFF,8+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CSTN3R	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,8+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,17+COFF
	FDB	2+COFF,10+COFF
	FDB	2+COFF,14+COFF
	FDB	3+COFF,14+COFF
	FDB	4+COFF,13+COFF
	FDB	5+COFF,12+COFF
	FDB	5+COFF,12+COFF
	FDB	6+COFF,11+COFF
	FDB	7+COFF,11+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,11+COFF
	FDB	5+COFF,12+COFF
	FDB	5+COFF,13+COFF
	FDB	5+COFF,14+COFF
	FDB	5+COFF,14+COFF
	FDB	5+COFF,14+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CSTN4R	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,8+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,17+COFF
	FDB	2+COFF,10+COFF
	FDB	2+COFF,14+COFF
	FDB	3+COFF,14+COFF
	FDB	4+COFF,13+COFF
	FDB	5+COFF,12+COFF
	FDB	5+COFF,12+COFF
	FDB	6+COFF,10+COFF
	FDB	4+COFF,10+COFF
	FDB	4+COFF,9+COFF
	FDB	4+COFF,9+COFF
	FDB	5+COFF,9+COFF
	FDB	5+COFF,10+COFF
	FDB	4+COFF,10+COFF
	FDB	4+COFF,10+COFF
	FDB	4+COFF,11+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CSKIDL	FDB	$8000,$8000	NO COLISION ON THIS LINE
	FDB	$8000,$8000	NO COLISION ON THIS LINE
	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	9+COFF,13+COFF
	FDB	9+COFF,17+COFF
	FDB	8+COFF,17+COFF
	FDB	8+COFF,16+COFF
	FDB	8+COFF,15+COFF
	FDB	8+COFF,14+COFF
	FDB	8+COFF,14+COFF
	FDB	8+COFF,13+COFF
	FDB	8+COFF,12+COFF
	FDB	8+COFF,12+COFF
	FDB	7+COFF,12+COFF
	FDB	6+COFF,12+COFF
	FDB	5+COFF,12+COFF
	FDB	4+COFF,13+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CSTN1L	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	2+COFF,13+COFF
	FDB	9+COFF,17+COFF
	FDB	5+COFF,17+COFF
	FDB	5+COFF,16+COFF
	FDB	6+COFF,15+COFF
	FDB	7+COFF,14+COFF
	FDB	7+COFF,14+COFF
	FDB	9+COFF,13+COFF
	FDB	9+COFF,15+COFF
	FDB	10+COFF,15+COFF
	FDB	10+COFF,13+COFF
	FDB	9+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	9+COFF,12+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CSTN2L	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	2+COFF,13+COFF
	FDB	9+COFF,17+COFF
	FDB	5+COFF,17+COFF
	FDB	5+COFF,16+COFF
	FDB	6+COFF,15+COFF
	FDB	7+COFF,14+COFF
	FDB	7+COFF,14+COFF
	FDB	9+COFF,13+COFF
	FDB	9+COFF,11+COFF
	FDB	9+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	5+COFF,13+COFF
	FDB	5+COFF,13+COFF
	FDB	4+COFF,13+COFF
	FDB	4+COFF,13+COFF
	FDB	11+COFF,13+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CSTN3L	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	2+COFF,13+COFF
	FDB	9+COFF,17+COFF
	FDB	5+COFF,17+COFF
	FDB	5+COFF,16+COFF
	FDB	6+COFF,15+COFF
	FDB	7+COFF,14+COFF
	FDB	7+COFF,14+COFF
	FDB	9+COFF,13+COFF
	FDB	8+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	8+COFF,13+COFF
	FDB	7+COFF,14+COFF
	FDB	6+COFF,14+COFF
	FDB	5+COFF,14+COFF
	FDB	5+COFF,14+COFF
	FDB	5+COFF,14+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
CSTN4L	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	2+COFF,13+COFF
	FDB	9+COFF,17+COFF
	FDB	5+COFF,17+COFF
	FDB	5+COFF,16+COFF
	FDB	6+COFF,15+COFF
	FDB	7+COFF,14+COFF
	FDB	7+COFF,14+COFF
	FDB	9+COFF,13+COFF
	FDB	9+COFF,15+COFF
	FDB	10+COFF,15+COFF
	FDB	10+COFF,15+COFF
	FDB	10+COFF,14+COFF
	FDB	9+COFF,14+COFF
	FDB	9+COFF,15+COFF
	FDB	9+COFF,15+COFF
	FDB	8+COFF,15+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
*
*	STORK IMAGES
*		(RIGHT)
*
*AAFF0A1478B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUN1R
  FDB	$0D10
	IF UseCompiledSprites
  FDB SRUN1RCS        * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FDB	$0000          * Original values of sprite
;  FCB $00
  ENDIF
;	FCB	$00,$00,$06,$D3,$00,$00
;	FCB	$00,$00,$00,$00,$00,$01,$94,$C4,$E0
;	FCB	$00,$00,$00,$00,$00,$09,$18,$00,$00
;	FCB	$00,$00,$00,$00,$00,$06,$D6,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$10,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$10,$00,$00
;	FCB	$00,$00,$00,$00,$06,$D1,$10,$00,$00
;	FCB	$00,$00,$06,$DD,$55,$11,$D0,$00,$00
;	FCB	$00,$00,$CD,$5D,$D5,$15,$60,$00,$00
;	FCB	$00,$05,$85,$51,$54,$1D,$60,$00,$00
;	FCB	$04,$84,$C4,$55,$8D,$56,$00,$00,$00
;	FCB	$04,$4C,$CC,$88,$D5,$80,$00,$00,$00
;	FCB	$00,$0D,$DD,$D1,$D8,$00,$00,$00,$00
;	FCB	$00,$DD,$88,$E8,$80,$00,$00,$00,$00
;	FCB	$00,$0E,$88,$08,$C0,$00,$00,$00,$00
;	FCB	$00,$08,$CC,$CC,$80,$00,$00,$00,$00
;	FCB	$00,$00,$00,$EE,$CC,$E0,$00,$00,$00
;	FCB	$00,$00,$00,$0C,$EE,$80,$00,$00,$00
;	FCB	$00,$00,$00,$0C,$80,$00,$00,$00,$00
;	FCB	$00,$00,$00,$08,$C6,$00,$00,$00,$00
*AAFF091478B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUN2R
	FDB	$0C10
	IF UseCompiledSprites
  FDB SRUN2RCS        * Jump address to draw compiled sprite
  FCB Sprites01        * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	FDB	$0000          * Original values of sprite
;  FCB $00
  ENDIF
;	FCB	$00,$00,$6D,$30,$00
;	FCB	$00,$00,$00,$00,$00,$19,$4C,$40
;	FCB	$00,$00,$00,$00,$00,$91,$80,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$D0,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$D0,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$10,$00
;	FCB	$00,$00,$00,$00,$06,$D1,$10,$00
;	FCB	$00,$00,$06,$DD,$55,$11,$50,$00
;	FCB	$00,$00,$CD,$5D,$D5,$15,$D0,$00
;	FCB	$00,$05,$85,$51,$5C,$15,$60,$00
;	FCB	$00,$84,$CC,$55,$8D,$56,$00,$00
;	FCB	$04,$44,$CC,$88,$DD,$80,$00,$00
;	FCB	$00,$0D,$DD,$D1,$D8,$00,$00,$00
;	FCB	$00,$DD,$86,$88,$80,$00,$00,$00
;	FCB	$00,$00,$0E,$8C,$CC,$E0,$00,$00
;	FCB	$00,$00,$0E,$C8,$EC,$C8,$00,$00
;	FCB	$00,$00,$08,$C0,$00,$EC,$00,$00
;	FCB	$00,$00,$08,$F0,$00,$08,$80,$00
;	FCB	$00,$00,$08,$CE,$00,$00,$80,$00
;	FCB	$00,$00,$0E,$CC,$00,$00,$00,$00
*AAFF091478B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUN3R:
	FDB	$0D10
	IF UseCompiledSprites
    FDB SRUN3RCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$6D,$30,$00,$00
;	FCB	$00,$00,$00,$00,$00,$19,$4C,$4E,$00
;	FCB	$00,$00,$00,$00,$00,$91,$80,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$D0,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$D0,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$10,$00,$00
;	FCB	$00,$00,$00,$00,$06,$D1,$10,$00,$00
;	FCB	$00,$00,$06,$DD,$55,$11,$50,$00,$00
;	FCB	$00,$00,$CD,$5D,$D5,$15,$D0,$00,$00
;	FCB	$00,$05,$85,$51,$5C,$15,$60,$00,$00
;	FCB	$00,$84,$CC,$55,$8D,$56,$00,$00,$00
;	FCB	$04,$44,$CC,$88,$DD,$80,$00,$00,$00
;	FCB	$00,$0D,$DD,$D1,$D8,$00,$00,$00,$00
;	FCB	$00,$DD,$60,$08,$88,$00,$00,$00,$00
;	FCB	$00,$00,$08,$C8,$EC,$80,$00,$00,$00
;	FCB	$00,$00,$EC,$80,$0C,$8E,$00,$00,$00
;	FCB	$00,$00,$8C,$80,$00,$CC,$E0,$00,$00
;	FCB	$00,$00,$8C,$E0,$00,$08,$CE,$00,$00
;	FCB	$00,$00,$C8,$00,$00,$0E,$88,$00,$00
;	FCB	$00,$00,$CC,$00,$00,$0E,$EC,$00,$00
*AAFF091478B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUN4R
	FDB	$0D10
	IF UseCompiledSprites
    FDB SRUN4RCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB STransRWhite-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB Sprites06       * Block where this sprite code is location
    FCB STransRBlue-STransRWhite       * Move this many bytes forward in the Jump table for coloured sprite
  ELSE
;	  FCB	$00,$00,$00,$00,$00,$06,$D3    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00,$01,$94,$C4,$00
;	FCB	$00,$00,$00,$00,$00,$09,$18,$00,$00
;	FCB	$00,$00,$00,$00,$00,$06,$D0,$00,$00
;	FCB	$00,$00,$00,$00,$00,$06,$10,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$10,$00,$00
;	FCB	$00,$00,$00,$00,$00,$D1,$10,$00,$00
;	FCB	$00,$00,$06,$DD,$55,$11,$D0,$00,$00
;	FCB	$00,$00,$CD,$5D,$D5,$15,$60,$00,$00
;	FCB	$00,$05,$85,$51,$5C,$1D,$60,$00,$00
;	FCB	$00,$84,$CC,$55,$8D,$56,$00,$00,$00
;	FCB	$04,$44,$CC,$88,$DD,$80,$00,$00,$00
;	FCB	$00,$0D,$DD,$D1,$D8,$00,$00,$00,$00
;	FCB	$00,$DD,$68,$8E,$88,$00,$00,$00,$00
;	FCB	$00,$0E,$CC,$08,$CE,$00,$00,$00,$00
;	FCB	$00,$08,$CE,$08,$C0,$00,$00,$00,$00
;	FCB	$00,$00,$C0,$00,$CE,$00,$00,$00,$00
;	FCB	$00,$00,$C0,$00,$88,$00,$00,$00,$00
;	FCB	$00,$08,$C0,$00,$EC,$00,$00,$00,$00
;	FCB	$00,$0C,$C0,$00,$0C,$C0,$00,$00,$00
*AAFF081378B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUNSR	FDB	$0C17
	IF UseCompiledSprites
    FDB SRUNSRCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$03,$D3,$00,$00
;	FCB	$00,$00,$00,$00,$0D,$94,$CE,$00
;	FCB	$00,$00,$00,$00,$09,$18,$84,$00
;	FCB	$00,$00,$00,$00,$00,$61,$60,$00
;	FCB	$00,$00,$00,$00,$00,$01,$D0,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$D0,$00
;	FCB	$00,$00,$00,$00,$00,$0D,$10,$00
;	FCB	$00,$00,$06,$DD,$55,$11,$50,$00
;	FCB	$00,$00,$CD,$5D,$D5,$15,$D0,$00
;	FCB	$00,$05,$85,$51,$5C,$15,$60,$00
;	FCB	$00,$84,$CC,$55,$8D,$56,$00,$00
;	FCB	$04,$44,$CC,$88,$DD,$80,$00,$00
;	FCB	$00,$0D,$DD,$D1,$D8,$00,$00,$00
;	FCB	$00,$DD,$68,$88,$C8,$00,$00,$00
;	FCB	$00,$00,$00,$8C,$EC,$80,$00,$00
;	FCB	$00,$00,$00,$08,$CE,$CE,$00,$00
;	FCB	$00,$00,$00,$00,$EC,$EC,$E0,$00
;	FCB	$00,$00,$00,$00,$0E,$CE,$CE,$00
;	FCB	$00,$00,$00,$00,$00,$CC,$EC,$00
*AAFF090E78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SFLY1R	FDB	$0D0A
	IF UseCompiledSprites
    FDB SFLY1RCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$6D,$30,$00,$00
;	FCB	$00,$00,$00,$00,$00,$19,$4C,$4E,$00
;	FCB	$00,$00,$00,$00,$00,$91,$80,$00,$00
;	FCB	$00,$00,$00,$00,$00,$01,$D0,$00,$00
;	FCB	$00,$00,$00,$00,$00,$01,$D0,$00,$00
;	FCB	$00,$00,$09,$77,$99,$0D,$10,$00,$00
;	FCB	$00,$00,$6D,$15,$DC,$6D,$10,$00,$00
;	FCB	$00,$06,$15,$51,$5C,$D1,$50,$00,$00
;	FCB	$00,$6D,$55,$15,$D6,$DD,$60,$00,$00
;	FCB	$00,$DD,$55,$5C,$58,$68,$00,$00,$00
;	FCB	$04,$86,$DC,$CC,$CC,$E0,$00,$00,$00
;	FCB	$08,$48,$68,$C4,$C4,$88,$00,$00,$00
;	FCB	$00,$80,$EE,$88,$4E,$CE,$C0,$00,$00
;	FCB	$00,$00,$00,$0E,$8E,$8E,$00,$00,$00
*AAFF0A0D78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SFLY3R	FDB	$0D09
	IF UseCompiledSprites
    FDB SFLY3RCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$C8,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00,$00
;	FCB	$0C,$04,$D0,$00,$00,$06,$D3,$00,$00
;	FCB	$00,$44,$DD,$00,$00,$0D,$94,$C4,$E0
;	FCB	$04,$48,$84,$90,$00,$09,$18,$00,$00
;	FCB	$00,$CC,$88,$C9,$00,$00,$1D,$00,$00
;	FCB	$0C,$CC,$CC,$85,$90,$00,$1D,$00,$00
;	FCB	$00,$0C,$5C,$8D,$58,$9D,$16,$00,$00
;	FCB	$00,$CC,$5D,$DC,$5D,$D1,$D6,$00,$00
;	FCB	$00,$08,$4D,$D1,$51,$11,$60,$00,$00
;	FCB	$00,$4C,$55,$11,$11,$D6,$00,$00,$00
;	FCB	$04,$C4,$58,$8C,$1D,$C0,$00,$00,$00
;	FCB	$04,$54,$0E,$E8,$88,$00,$00,$00,$00
;	FCB	$00,$00,$88,$E0,$08,$00,$00,$00,$00
*
*	(LEFT)
*	STORK LEFT FACED IMAGES
*
*AAFF0A1478B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUN1L	FDB	$0D10
	IF UseCompiledSprites
    FDB SRUN1LCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$3D    * Original values of sprite
  ENDIF
;	FCB	$60,$00,$00,$00,$00,$00
;	FCB	$0E,$4C,$49,$10,$00,$00,$00,$00,$00
;	FCB	$00,$00,$81,$90,$00,$00,$00,$00,$00
;	FCB	$00,$00,$6D,$60,$00,$00,$00,$00,$00
;	FCB	$00,$00,$01,$D0,$00,$00,$00,$00,$00
;	FCB	$00,$00,$01,$D0,$00,$00,$00,$00,$00
;	FCB	$00,$00,$01,$1D,$60,$00,$00,$00,$00
;	FCB	$00,$00,$0D,$11,$55,$DD,$60,$00,$00
;	FCB	$00,$00,$06,$51,$5D,$D5,$DC,$00,$00
;	FCB	$00,$00,$06,$D1,$45,$15,$58,$50,$00
;	FCB	$00,$00,$00,$65,$D8,$55,$4C,$48,$40
;	FCB	$00,$00,$00,$08,$5D,$88,$CC,$C4,$40
;	FCB	$00,$00,$00,$00,$8D,$1D,$DD,$D0,$00
;	FCB	$00,$00,$00,$00,$08,$8E,$88,$DD,$00
;	FCB	$00,$00,$00,$00,$0C,$80,$88,$E0,$00
;	FCB	$00,$00,$00,$00,$08,$CC,$CC,$80,$00
;	FCB	$00,$00,$00,$0E,$CC,$EE,$00,$00,$00
;	FCB	$00,$00,$00,$08,$EE,$C0,$00,$00,$00
;	FCB	$00,$00,$00,$00,$08,$C0,$00,$00,$00
;	FCB	$00,$00,$00,$00,$6C,$80,$00,$00,$00
*AAFF091478B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUN2L	FDB	$0C10
	IF UseCompiledSprites
    FDB SRUN2LCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$03,$D6    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00
;	FCB	$04,$C4,$91,$00,$00,$00,$00,$00
;	FCB	$00,$08,$19,$00,$00,$00,$00,$00
;	FCB	$00,$0D,$D0,$00,$00,$00,$00,$00
;	FCB	$00,$0D,$D0,$00,$00,$00,$00,$00
;	FCB	$00,$01,$D0,$00,$00,$00,$00,$00
;	FCB	$00,$01,$1D,$60,$00,$00,$00,$00
;	FCB	$00,$05,$11,$55,$DD,$60,$00,$00
;	FCB	$00,$0D,$51,$5D,$D5,$DC,$00,$00
;	FCB	$00,$06,$51,$C5,$15,$58,$50,$00
;	FCB	$00,$00,$65,$D8,$55,$CC,$48,$00
;	FCB	$00,$00,$08,$DD,$88,$CC,$44,$40
;	FCB	$00,$00,$00,$8D,$1D,$DD,$D0,$00
;	FCB	$00,$00,$00,$08,$88,$68,$DD,$00
;	FCB	$00,$00,$0E,$CC,$C8,$E0,$00,$00
;	FCB	$00,$00,$8C,$CE,$8C,$E0,$00,$00
;	FCB	$00,$00,$CE,$00,$0C,$80,$00,$00
;	FCB	$00,$08,$80,$00,$0F,$80,$00,$00
;	FCB	$00,$08,$00,$00,$EC,$80,$00,$00
;	FCB	$00,$00,$00,$00,$CC,$E0,$00,$00
*AAFF091478B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUN3L	FDB	$0C10
	IF UseCompiledSprites
    FDB SRUN3LCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$03,$D6    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00
;	FCB	$E4,$C4,$91,$00,$00,$00,$00,$00
;	FCB	$00,$08,$19,$00,$00,$00,$00,$00
;	FCB	$00,$0D,$D0,$00,$00,$00,$00,$00
;	FCB	$00,$0D,$D0,$00,$00,$00,$00,$00
;	FCB	$00,$01,$D0,$00,$00,$00,$00,$00
;	FCB	$00,$01,$1D,$60,$00,$00,$00,$00
;	FCB	$00,$05,$11,$55,$DD,$60,$00,$00
;	FCB	$00,$0D,$51,$5D,$D5,$DC,$00,$00
;	FCB	$00,$06,$51,$C5,$15,$58,$50,$00
;	FCB	$00,$00,$65,$D8,$55,$CC,$48,$00
;	FCB	$00,$00,$08,$DD,$88,$CC,$44,$40
;	FCB	$00,$00,$00,$8D,$1D,$DD,$D0,$00
;	FCB	$00,$00,$00,$88,$80,$06,$DD,$00
;	FCB	$00,$00,$08,$CE,$8C,$80,$00,$00
;	FCB	$00,$00,$E8,$C0,$08,$CE,$00,$00
;	FCB	$00,$0E,$CC,$00,$08,$C8,$00,$00
;	FCB	$00,$EC,$80,$00,$0E,$C8,$00,$00
;	FCB	$00,$88,$E0,$00,$00,$8C,$00,$00
;	FCB	$00,$CE,$E0,$00,$00,$CC,$00,$00
*AAFF091478B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUN4L	FDB	$0C10
	IF UseCompiledSprites
    FDB SRUN4LCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB STransLWhite-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB Sprites06       * Block where this sprite code is location
    FCB STransLBlue-STransLWhite       * Move this many bytes forward in the Jump table for coloured sprite
  ELSE
;	  FCB	$00,$3D,$60,$00,$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$4C,$49,$10,$00,$00,$00,$00,$00
;	FCB	$00,$81,$90,$00,$00,$00,$00,$00
;	FCB	$00,$0D,$60,$00,$00,$00,$00,$00
;	FCB	$00,$01,$60,$00,$00,$00,$00,$00
;	FCB	$00,$01,$D0,$00,$00,$00,$00,$00
;	FCB	$00,$01,$1D,$00,$00,$00,$00,$00
;	FCB	$00,$0D,$11,$55,$DD,$60,$00,$00
;	FCB	$00,$06,$51,$5D,$D5,$DC,$00,$00
;	FCB	$00,$06,$D1,$C5,$15,$58,$50,$00
;	FCB	$00,$00,$65,$D8,$55,$CC,$48,$00
;	FCB	$00,$00,$08,$DD,$88,$CC,$44,$40
;	FCB	$00,$00,$00,$8D,$1D,$DD,$D0,$00
;	FCB	$00,$00,$00,$88,$E8,$86,$DD,$00
;	FCB	$00,$00,$00,$EC,$80,$CC,$E0,$00
;	FCB	$00,$00,$00,$0C,$80,$EC,$80,$00
;	FCB	$00,$00,$00,$EC,$00,$0C,$00,$00
;	FCB	$00,$00,$00,$88,$00,$0C,$00,$00
;	FCB	$00,$00,$00,$CE,$00,$0C,$80,$00
;	FCB	$00,$00,$0C,$C0,$00,$0C,$C0,$00
*AAFF081378B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SRUNSL	FDB	$0317
	IF UseCompiledSprites
    FDB SRUNSLCS        * Jump address to draw compiled sprite
    FCB Sprites01      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$3D,$30    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00
;	FCB	$EC,$49,$D0,$00,$00,$00,$00
;	FCB	$48,$81,$90,$00,$00,$00,$00
;	FCB	$06,$16,$00,$00,$00,$00,$00
;	FCB	$0D,$10,$00,$00,$00,$00,$00
;	FCB	$0D,$D0,$00,$00,$00,$00,$00
;	FCB	$01,$D0,$00,$00,$00,$00,$00
;	FCB	$05,$11,$55,$DD,$60,$00,$00
;	FCB	$0D,$51,$5D,$D5,$DC,$00,$00
;	FCB	$06,$51,$C5,$15,$58,$50,$00
;	FCB	$00,$65,$D8,$55,$CC,$48,$00
;	FCB	$00,$08,$DD,$88,$CC,$44,$40
;	FCB	$00,$00,$8D,$1D,$DD,$D0,$00
;	FCB	$00,$00,$8C,$88,$86,$DD,$00
;	FCB	$00,$08,$CE,$C8,$00,$00,$00
;	FCB	$00,$EC,$EC,$80,$00,$00,$00
;	FCB	$0E,$CE,$CE,$00,$00,$00,$00
;	FCB	$EC,$EC,$E0,$00,$00,$00,$00
;	FCB	$CE,$CC,$00,$00,$00,$00,$00
*AAFF090E78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SFLY1L	FDB	$0C0A
	IF UseCompiledSprites
    FDB SFLY1LCS        * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$03,$D6    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00
;	FCB	$E4,$C4,$91,$00,$00,$00,$00,$00
;	FCB	$00,$08,$19,$00,$00,$00,$00,$00
;	FCB	$00,$0D,$10,$00,$00,$00,$00,$00
;	FCB	$00,$0D,$10,$00,$00,$00,$00,$00
;	FCB	$00,$01,$D0,$99,$77,$90,$00,$00
;	FCB	$00,$01,$D6,$CD,$51,$D6,$00,$00
;	FCB	$00,$05,$1D,$C5,$15,$51,$60,$00
;	FCB	$00,$06,$DD,$6D,$51,$55,$D6,$00
;	FCB	$00,$00,$86,$85,$C5,$55,$DD,$00
;	FCB	$00,$00,$0E,$CC,$CC,$CD,$68,$40
;	FCB	$00,$00,$88,$4C,$4C,$86,$84,$80
;	FCB	$00,$0C,$EC,$E4,$88,$EE,$08,$00
;	FCB	$00,$00,$E8,$E8,$E0,$00,$00,$00
*AAFF0A0D78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
SFLY3L	FDB	$0D09
	IF UseCompiledSprites
    FDB SFLY3LCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$8C,$00
;	FCB	$00,$00,$3D,$60,$00,$00,$0D,$40,$C0
;	FCB	$0E,$4C,$49,$D0,$00,$00,$DD,$44,$00
;	FCB	$00,$00,$81,$90,$00,$09,$48,$84,$40
;	FCB	$00,$00,$D1,$00,$00,$9C,$88,$CC,$00
;	FCB	$00,$00,$D1,$00,$09,$58,$CC,$CC,$C0
;	FCB	$00,$00,$61,$D9,$85,$D8,$C5,$C0,$00
;	FCB	$00,$00,$6D,$1D,$D5,$CD,$D5,$CC,$00
;	FCB	$00,$00,$06,$11,$15,$1D,$D4,$80,$00
;	FCB	$00,$00,$00,$6D,$11,$11,$55,$C4,$00
;	FCB	$00,$00,$00,$0C,$D1,$C8,$85,$4C,$40
;	FCB	$00,$00,$00,$00,$88,$8E,$E0,$45,$40
;	FCB	$00,$00,$00,$00,$80,$0E,$88,$00,$00
*
*	OSTRICH
*	 (RIGHT)
*
*AA00FF70580F3F51E81490CD111FA40A670914
ORUN1R	FDB	$0C10
	IF UseCompiledSprites
    FDB ORUN1RCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$08,$F8,$00
;	FCB	$00,$00,$00,$00,$00,$0F,$7F,$50
;	FCB	$00,$00,$00,$00,$00,$08,$F0,$00
;	FCB	$00,$00,$00,$00,$00,$0E,$FE,$00
;	FCB	$00,$00,$00,$00,$00,$00,$8F,$00
;	FCB	$09,$70,$00,$00,$00,$00,$8F,$00
;	FCB	$07,$17,$97,$66,$66,$00,$8F,$00
;	FCB	$00,$69,$76,$71,$77,$98,$FE,$00
;	FCB	$00,$06,$96,$07,$97,$7F,$80,$00
;	FCB	$00,$00,$97,$96,$77,$79,$00,$00
;	FCB	$00,$00,$66,$98,$99,$60,$00,$00
;	FCB	$00,$00,$88,$8E,$88,$00,$00,$00
;	FCB	$00,$0F,$F8,$00,$F8,$00,$00,$00
;	FCB	$00,$0E,$FF,$08,$F0,$00,$00,$00
;	FCB	$00,$00,$08,$FF,$80,$00,$00,$00
;	FCB	$00,$00,$00,$EF,$FE,$00,$00,$00
;	FCB	$00,$00,$00,$EF,$88,$00,$00,$00
;	FCB	$00,$00,$00,$EF,$80,$00,$00,$00
;	FCB	$00,$00,$00,$EF,$80,$00,$00,$00
;	FCB	$00,$00,$00,$E8,$F8,$00,$00,$00
*
*AA00FF70580F3F51E81490CD111FA40A670914
ORUN2R	FDB	$0C10
	IF UseCompiledSprites
    FDB ORUN2RCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$8F,$80,$00
;	FCB	$00,$00,$00,$00,$00,$F7,$F5,$00
;	FCB	$00,$00,$00,$00,$00,$8F,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0F,$80,$00
;	FCB	$00,$00,$00,$00,$00,$08,$F0,$00
;	FCB	$09,$70,$00,$00,$00,$00,$8F,$00
;	FCB	$07,$17,$97,$66,$66,$00,$8F,$00
;	FCB	$00,$69,$76,$71,$77,$98,$F8,$00
;	FCB	$00,$06,$96,$07,$97,$7F,$80,$00
;	FCB	$00,$00,$97,$99,$11,$77,$00,$00
;	FCB	$00,$00,$69,$97,$77,$96,$00,$00
;	FCB	$00,$00,$06,$88,$88,$60,$00,$00
;	FCB	$00,$00,$00,$0F,$F8,$00,$00,$00
;	FCB	$00,$00,$00,$8F,$8E,$00,$00,$00
;	FCB	$00,$00,$0E,$8F,$F8,$00,$00,$00
;	FCB	$00,$00,$0E,$F8,$0F,$F8,$00,$00
;	FCB	$00,$00,$08,$F0,$00,$EF,$00,$00
;	FCB	$00,$00,$08,$F0,$00,$08,$80,$00
;	FCB	$00,$00,$08,$F8,$00,$00,$80,$00
;	FCB	$00,$00,$0E,$FF,$00,$00,$00,$00
*
*AA00FF70580F3F51E81490CD111EA40A670914
ORUN3R	FDB	$0C10
	IF UseCompiledSprites
    FDB ORUN3RCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$8F,$80,$00
;	FCB	$00,$00,$00,$00,$00,$F7,$F5,$00
;	FCB	$00,$00,$00,$00,$00,$8F,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0F,$80,$00
;	FCB	$00,$00,$00,$00,$00,$08,$F0,$00
;	FCB	$09,$70,$00,$00,$00,$00,$8F,$00
;	FCB	$07,$17,$97,$67,$76,$00,$8F,$00
;	FCB	$00,$69,$70,$71,$77,$98,$F8,$00
;	FCB	$00,$06,$96,$67,$97,$7F,$80,$00
;	FCB	$00,$00,$97,$71,$17,$79,$00,$00
;	FCB	$00,$00,$69,$67,$77,$96,$00,$00
;	FCB	$00,$00,$06,$88,$88,$60,$00,$00
;	FCB	$00,$00,$00,$F8,$FF,$E0,$00,$00
;	FCB	$00,$00,$08,$F8,$EF,$00,$00,$00
;	FCB	$00,$00,$0F,$80,$EF,$80,$00,$00
;	FCB	$00,$00,$EF,$80,$08,$F8,$00,$00
;	FCB	$00,$00,$8F,$E0,$00,$8F,$E0,$00
;	FCB	$00,$00,$8F,$00,$00,$08,$FE,$00
;	FCB	$00,$00,$F8,$00,$00,$0E,$88,$00
;	FCB	$00,$00,$FF,$00,$00,$0E,$EF,$00
*
*AA00FF70580F3F51E8149018111FA40A670914
ORUN4R	FDB	$0C10
	IF UseCompiledSprites
    FDB ORUN4RCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB OTransRWhite-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB Sprites05       * Block where this sprite code is location
    FCB OTransRYellow-OTransRWhite       * Move this many bytes forward in the Jump table for coloured sprite
  ELSE
;	  FCB	$00,$00,$00,$00,$00,$08,$F8    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$00,$00,$00,$00,$00,$0F,$7F,$50
;	FCB	$00,$00,$00,$00,$00,$08,$F0,$00
;	FCB	$00,$00,$00,$00,$00,$0E,$F0,$00
;	FCB	$00,$00,$00,$00,$00,$00,$8F,$00
;	FCB	$09,$70,$00,$00,$00,$00,$8F,$00
;	FCB	$07,$17,$97,$66,$66,$00,$8F,$00
;	FCB	$00,$69,$76,$71,$17,$98,$F8,$00
;	FCB	$00,$06,$96,$07,$77,$98,$80,$00
;	FCB	$00,$00,$67,$96,$76,$69,$00,$00
;	FCB	$00,$00,$66,$98,$99,$60,$00,$00
;	FCB	$00,$00,$88,$8E,$88,$00,$00,$00
;	FCB	$00,$0E,$FF,$00,$F8,$00,$00,$00
;	FCB	$00,$08,$FE,$08,$F0,$00,$00,$00
;	FCB	$00,$0F,$F0,$08,$F0,$00,$00,$00
;	FCB	$00,$08,$F0,$08,$F0,$00,$00,$00
;	FCB	$00,$00,$F0,$00,$FE,$00,$00,$00
;	FCB	$00,$0E,$F0,$00,$88,$00,$00,$00
;	FCB	$00,$08,$F0,$00,$EF,$00,$00,$00
;	FCB	$00,$08,$F0,$00,$0F,$F0,$00,$00
*
*AA00FF70580F3F51E81490CD111FA40A670911
ORUNSR	FDB	$0C16
	IF UseCompiledSprites
    FDB ORUNSRCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$0E,$88,$00
;	FCB	$00,$00,$00,$00,$00,$00,$7F,$50
;	FCB	$00,$00,$00,$00,$00,$00,$FF,$00
;	FCB	$00,$00,$00,$00,$00,$00,$88,$00
;	FCB	$00,$97,$00,$00,$00,$00,$8F,$00
;	FCB	$09,$19,$69,$70,$00,$00,$8F,$00
;	FCB	$09,$97,$97,$71,$17,$AA,$8F,$00
;	FCB	$00,$61,$70,$71,$11,$9A,$FF,$00
;	FCB	$00,$07,$99,$66,$97,$7F,$F8,$00
;	FCB	$00,$00,$97,$71,$77,$7F,$80,$00
;	FCB	$00,$00,$67,$1F,$F7,$76,$00,$00
;	FCB	$00,$00,$06,$8F,$F8,$80,$00,$00
;	FCB	$00,$00,$00,$0F,$F8,$E0,$00,$00
;	FCB	$00,$00,$00,$00,$F8,$F0,$00,$00
;	FCB	$00,$00,$00,$00,$EF,$8F,$00,$00
;	FCB	$00,$00,$00,$00,$08,$F8,$F0,$00
;	FCB	$00,$00,$00,$00,$00,$EF,$EF,$00
;	FCB	$00,$00,$00,$00,$00,$0F,$FE,$F0
*
*AAFF0A0D78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
OFLY1R	FDB	$0D09
	IF UseCompiledSprites
    FDB OFLY1RCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$08,$F8,$00
;	FCB	$00,$00,$00,$00,$00,$00,$0F,$7F,$50
;	FCB	$00,$00,$00,$00,$00,$00,$0F,$80,$00
;	FCB	$00,$00,$00,$00,$00,$00,$0F,$80,$00
;	FCB	$00,$00,$00,$00,$00,$00,$0F,$E0,$00
;	FCB	$00,$69,$77,$79,$96,$00,$EF,$E0,$00
;	FCB	$09,$61,$19,$77,$79,$9E,$8F,$00,$00
;	FCB	$00,$66,$07,$17,$77,$98,$F8,$00,$00
;	FCB	$00,$09,$61,$76,$71,$98,$80,$00,$00
;	FCB	$00,$00,$66,$06,$17,$99,$00,$00,$00
;	FCB	$00,$00,$EE,$99,$99,$9E,$00,$00,$00
;	FCB	$00,$00,$E8,$88,$EE,$8E,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$E8,$00,$00,$00
*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE
*
	FCC	'JOUST (C) 1982 WILLIAMS ELECTRONICS INC.'
*AAFF090D78B074F774F67476F4F67474
*00FF70580F3F51E81490CD111FA40A67
OFLY3R	FDB	$0D09
	IF UseCompiledSprites
    FDB OFLY3RCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$07,$96,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$8F,$80,$00
;	FCB	$01,$17,$79,$00,$00,$00,$F7,$F5,$00
;	FCB	$00,$77,$79,$00,$00,$00,$F8,$00,$00
;	FCB	$00,$11,$79,$00,$00,$00,$F8,$00,$00
;	FCB	$00,$07,$77,$90,$00,$00,$F8,$00,$00
;	FCB	$00,$01,$17,$97,$00,$00,$F8,$00,$00
;	FCB	$00,$00,$77,$77,$79,$08,$C8,$00,$00
;	FCB	$09,$D7,$60,$7D,$D7,$9F,$FE,$00,$00
;	FCB	$09,$69,$96,$6D,$D6,$9F,$E0,$00,$00
;	FCB	$00,$00,$99,$96,$69,$98,$00,$00,$00
;	FCB	$00,$00,$EE,$99,$99,$9E,$00,$00,$00
;	FCB	$00,$00,$E8,$88,$EE,$8E,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$E8,$00,$00,$00
*
*	OSTRICH
*	 (LEFT)
*
*AA00FF70580F3F51E81490CD111FA40A670914
ORUN1L	FDB	$0C10
	IF UseCompiledSprites
    FDB ORUN1LCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$8F,$80    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00
;	FCB	$05,$F7,$F0,$00,$00,$00,$00,$00
;	FCB	$00,$0F,$80,$00,$00,$00,$00,$00
;	FCB	$00,$EF,$E0,$00,$00,$00,$00,$00
;	FCB	$00,$F8,$00,$00,$00,$00,$00,$00
;	FCB	$00,$F8,$00,$00,$00,$00,$07,$90
;	FCB	$00,$F8,$00,$66,$66,$79,$71,$70
;	FCB	$00,$EF,$89,$77,$17,$67,$96,$00
;	FCB	$00,$08,$F7,$79,$70,$69,$60,$00
;	FCB	$00,$00,$97,$77,$69,$79,$00,$00
;	FCB	$00,$00,$06,$99,$89,$66,$00,$00
;	FCB	$00,$00,$00,$88,$E8,$88,$00,$00
;	FCB	$00,$00,$00,$8F,$00,$8F,$F0,$00
;	FCB	$00,$00,$00,$0F,$80,$FF,$E0,$00
;	FCB	$00,$00,$00,$08,$FF,$80,$00,$00
;	FCB	$00,$00,$00,$EF,$FE,$00,$00,$00
;	FCB	$00,$00,$00,$88,$FE,$00,$00,$00
;	FCB	$00,$00,$00,$08,$FE,$00,$00,$00
;	FCB	$00,$00,$00,$08,$FE,$00,$00,$00
;	FCB	$00,$00,$00,$8F,$8E,$00,$00,$00
*
*AA00FF70580F3F51E81490CD111FA40A670914

;    ORG   $2000     - remove the semicolon to draw the ostrich running properly, but screws up the pointers so they don't match with real joust

ORUN2L	FDB	$0310
	IF UseCompiledSprites
    FDB ORUN2LCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$08,$F8,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00
;	FCB	$5F,$7F,$00,$00,$00,$00,$00
;	FCB	$00,$F8,$00,$00,$00,$00,$00
;	FCB	$08,$F0,$00,$00,$00,$00,$00
;	FCB	$0F,$80,$00,$00,$00,$00,$00
;	FCB	$F8,$00,$00,$00,$00,$07,$90
;	FCB	$F8,$00,$66,$66,$79,$71,$70
;	FCB	$8F,$89,$77,$17,$67,$96,$00
;	FCB	$08,$F7,$79,$70,$69,$60,$00
;	FCB	$00,$77,$11,$99,$79,$00,$00
;	FCB	$00,$69,$77,$79,$96,$00,$00
;	FCB	$00,$06,$88,$88,$60,$00,$00
;	FCB	$00,$00,$8F,$F0,$00,$00,$00
;	FCB	$00,$00,$E8,$F8,$00,$00,$00
;	FCB	$00,$00,$8F,$F8,$E0,$00,$00
;	FCB	$00,$8F,$F0,$8F,$E0,$00,$00
;	FCB	$00,$FE,$00,$0F,$80,$00,$00
;	FCB	$08,$80,$00,$0F,$80,$00,$00
;	FCB	$08,$00,$00,$8F,$80,$00,$00
;	FCB	$00,$00,$00,$FF,$E0,$00,$00
*
*AA00FF70580F3F51E81490CD111EA40A670914
ORUN3L	FDB	$0310
	IF UseCompiledSprites
    FDB ORUN3LCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$08,$F8,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00
;	FCB	$5F,$7F,$00,$00,$00,$00,$00
;	FCB	$00,$F8,$00,$00,$00,$00,$00
;	FCB	$08,$F0,$00,$00,$00,$00,$00
;	FCB	$0F,$80,$00,$00,$00,$00,$00
;	FCB	$F8,$00,$00,$00,$00,$07,$90
;	FCB	$F8,$00,$67,$76,$79,$71,$70
;	FCB	$8F,$89,$77,$17,$07,$96,$00
;	FCB	$08,$F7,$79,$76,$69,$60,$00
;	FCB	$00,$97,$71,$17,$79,$00,$00
;	FCB	$00,$69,$77,$76,$96,$00,$00
;	FCB	$00,$06,$88,$88,$60,$00,$00
;	FCB	$00,$0E,$FF,$8F,$00,$00,$00
;	FCB	$00,$00,$FE,$8F,$80,$00,$00
;	FCB	$00,$08,$FE,$08,$F0,$00,$00
;	FCB	$00,$8F,$80,$08,$FE,$00,$00
;	FCB	$0E,$F8,$00,$0E,$F8,$00,$00
;	FCB	$EF,$80,$00,$00,$F8,$00,$00
;	FCB	$88,$E0,$00,$00,$8F,$00,$00
;	FCB	$FE,$E0,$00,$00,$FF,$00,$00
*
*AA00FF70580F3F51E8149018111FA40A670914
ORUN4L	FDB	$0C10
	IF UseCompiledSprites
    FDB ORUN4LCS        * Jump address to draw compiled sprite
    FCB Sprites02       * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB OTransLWhite-2  * Table address for a transporter sprite - 2 because there is no size 0
    FCB Sprites05       * Block where this sprite code is location
    FCB OTransLYellow-OTransLWhite       * Move this many bytes forward in the Jump table for coloured sprite
  ELSE
;	  FCB	$00,$8F,$80,$00,$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$05,$F7,$F0,$00,$00,$00,$00,$00
;	FCB	$00,$0F,$80,$00,$00,$00,$00,$00
;	FCB	$00,$0F,$E0,$00,$00,$00,$00,$00
;	FCB	$00,$F8,$00,$00,$00,$00,$00,$00
;	FCB	$00,$F8,$00,$00,$00,$00,$07,$90
;	FCB	$00,$F8,$00,$66,$66,$79,$71,$70
;	FCB	$00,$8F,$89,$71,$17,$67,$96,$00
;	FCB	$00,$08,$89,$77,$70,$69,$60,$00
;	FCB	$00,$00,$96,$67,$69,$76,$00,$00
;	FCB	$00,$00,$06,$99,$89,$66,$00,$00
;	FCB	$00,$00,$00,$88,$E8,$88,$00,$00
;	FCB	$00,$00,$00,$8F,$00,$FF,$E0,$00
;	FCB	$00,$00,$00,$0F,$80,$EF,$80,$00
;	FCB	$00,$00,$00,$0F,$80,$0F,$F0,$00
;	FCB	$00,$00,$00,$0F,$80,$0F,$80,$00
;	FCB	$00,$00,$00,$EF,$00,$0F,$00,$00
;	FCB	$00,$00,$00,$88,$00,$0F,$E0,$00
;	FCB	$00,$00,$00,$FE,$00,$0F,$80,$00
;	FCB	$00,$00,$0F,$F0,$00,$0F,$80,$00
*
*AA00FF70580F3F51E81490CD111FA40A670911
ORUNSL	FDB	$0C16
	IF UseCompiledSprites
    FDB ORUNSLCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$88,$E0    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00
;	FCB	$05,$F7,$00,$00,$00,$00,$00,$00
;	FCB	$00,$FF,$00,$00,$00,$00,$00,$00
;	FCB	$00,$88,$00,$00,$00,$00,$00,$00
;	FCB	$00,$F8,$00,$00,$00,$00,$79,$00
;	FCB	$00,$F8,$00,$00,$07,$96,$91,$90
;	FCB	$00,$F8,$AA,$71,$17,$79,$79,$90
;	FCB	$00,$FF,$A9,$11,$17,$07,$16,$00
;	FCB	$00,$8F,$F7,$79,$66,$99,$70,$00
;	FCB	$00,$08,$F7,$77,$17,$79,$00,$00
;	FCB	$00,$00,$67,$7F,$F1,$76,$00,$00
;	FCB	$00,$00,$08,$8F,$F8,$60,$00,$00
;	FCB	$00,$00,$0E,$8F,$F0,$00,$00,$00
;	FCB	$00,$00,$0F,$8F,$00,$00,$00,$00
;	FCB	$00,$00,$F8,$FE,$00,$00,$00,$00
;	FCB	$00,$0F,$8F,$80,$00,$00,$00,$00
;	FCB	$00,$FE,$FE,$00,$00,$00,$00,$00
;	FCB	$0F,$EF,$F0,$00,$00,$00,$00,$00
*
*AAFF0A0D78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
OFLY1L	FDB	$0D09
	IF UseCompiledSprites
    FDB OFLY1LCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$8F,$80    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00,$00
;	FCB	$05,$F7,$F0,$00,$00,$00,$00,$00,$00
;	FCB	$00,$08,$F0,$00,$00,$00,$00,$00,$00
;	FCB	$00,$08,$F0,$00,$00,$00,$00,$00,$00
;	FCB	$00,$0E,$F0,$00,$00,$00,$00,$00,$00
;	FCB	$00,$0E,$FE,$00,$69,$97,$77,$96,$00
;	FCB	$00,$00,$F8,$E9,$97,$77,$91,$16,$90
;	FCB	$00,$00,$8F,$89,$77,$71,$70,$66,$00
;	FCB	$00,$00,$08,$89,$17,$67,$16,$90,$00
;	FCB	$00,$00,$00,$99,$71,$60,$66,$00,$00
;	FCB	$00,$00,$00,$E9,$99,$99,$EE,$00,$00
;	FCB	$00,$00,$00,$E8,$EE,$88,$8E,$00,$00
;	FCB	$00,$00,$00,$8E,$00,$00,$00,$00,$00
*AAFF090D78B074F774F67476F4F67474
*00FF70580F3F51E81490CD111FA40A67
OFLY3L	FDB	$0D09
	IF UseCompiledSprites
    FDB OFLY3LCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$08,$F8    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$69,$70
;	FCB	$00,$5F,$7F,$00,$00,$00,$97,$71,$10
;	FCB	$00,$00,$8F,$00,$00,$00,$97,$77,$00
;	FCB	$00,$00,$8F,$00,$00,$00,$97,$11,$00
;	FCB	$00,$00,$8F,$00,$00,$09,$77,$70,$00
;	FCB	$00,$00,$8F,$00,$00,$79,$71,$10,$00
;	FCB	$00,$00,$8C,$80,$97,$77,$77,$00,$00
;	FCB	$00,$00,$EF,$F9,$7D,$D7,$06,$7D,$90
;	FCB	$00,$00,$0E,$F9,$6D,$D6,$69,$96,$90
;	FCB	$00,$00,$00,$89,$96,$69,$99,$00,$00
;	FCB	$00,$00,$00,$E9,$99,$99,$EE,$00,$00
;	FCB	$00,$00,$00,$E8,$EE,$88,$8E,$00,$00
;	FCB	$00,$00,$00,$8E,$00,$00,$00,$00,$00
*
*	BUZZARD IMAGES
*
BUZARD	EQU	*
BRSKID	FDB	BSKIDR,$00F4,BRUNSR
	FDB	BSKIDL,$01F4,BRUNSL
BRSTND	FDB	BSTNDR,$00F3,BRUN4R	BSTNDR,$00F3,BRUN0R
	FDB	BSTNDL,$00F3,BRUN4L	BSTNDL,$00F3,BRUN0L
BRRUN1	FDB	BSTNDR,$00F3,BRUN1R
	FDB	BSTNDL,$00F3,BRUN1L
BRRUN2	FDB	BSTNDR,$00F3,BRUN2R
	FDB	BSTNDL,$01F3,BRUN2L
BRRUN3	FDB	BSTNDR,$00F3,BRUN3R
	FDB	BSTNDL,$01F3,BRUN3L
BRRUN4	FDB	BSTNDR,$00F3,BRUN4R
	FDB	BSTNDL,$00F3,BRUN4L
BRFLAP	FDB	BWNG1R,$00F2,BFLY1R
	FDB	BWNG1L,$00F2,BFLY1L
BRFLOP	FDB	BWNG2R,$00ED,BFLY1R	,BFLY2R
	FDB	BWNG2L,$00ED,BFLY1L	,BFLY2L
BRFLIP	FDB	BWNG3R,$00ED,BFLY3R
	FDB	BWNG3L,$00ED,BFLY3L
*
*	BUZZARDS COLISION DETECT DATA
*
BWNG1R
BWNG2R
BWNG3R	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,8+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,17+COFF
	FDB	6+COFF,10+COFF
	FDB	5+COFF,11+COFF
	FDB	4+COFF,14+COFF
	FDB	3+COFF,14+COFF
	FDB	4+COFF,14+COFF
	FDB	5+COFF,13+COFF
	FDB	5+COFF,12+COFF
	FDB	5+COFF,13+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
BWNG1L
BWNG2L
BWNG3L	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	2+COFF,13+COFF
	FDB	9+COFF,13+COFF
	FDB	8+COFF,14+COFF
	FDB	5+COFF,15+COFF
	FDB	5+COFF,16+COFF
	FDB	5+COFF,15+COFF
	FDB	6+COFF,14+COFF
	FDB	7+COFF,14+COFF
	FDB	6+COFF,14+COFF
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
BSKIDR	FDB	$8000,$8000	NO COLISION ON THIS LINE
	FDB	$8000,$8000	NO COLISION ON THIS LINE
	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,8+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,11+COFF
	FDB	5+COFF,11+COFF
	FDB	5+COFF,11+COFF
	FDB	3+COFF,11+COFF
	FDB	3+COFF,11+COFF
	FDB	3+COFF,11+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
BSTNDR	FDB	7+COFF,9+COFF
	FDB	7+COFF,9+COFF
	FDB	7+COFF,8+COFF
	FDB	6+COFF,10+COFF
	FDB	6+COFF,17+COFF
	FDB	7+COFF,10+COFF
	FDB	6+COFF,14+COFF
	FDB	5+COFF,14+COFF
	FDB	4+COFF,14+COFF
	FDB	3+COFF,14+COFF
	FDB	3+COFF,14+COFF
	FDB	2+COFF,14+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
BSKIDL	FDB	$8000,$8000	NO COLISION ON THIS LINE
	FDB	$8000,$8000	NO COLISION ON THIS LINE
	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	9+COFF,13+COFF
	FDB	9+COFF,13+COFF
	FDB	8+COFF,13+COFF
	FDB	8+COFF,14+COFF
	FDB	8+COFF,14+COFF
	FDB	8+COFF,16+COFF
	FDB	8+COFF,16+COFF
	FDB	8+COFF,16+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
BSTNDL	FDB	10+COFF,12+COFF
	FDB	10+COFF,12+COFF
	FDB	11+COFF,12+COFF
	FDB	9+COFF,13+COFF
	FDB	2+COFF,13+COFF
	FDB	9+COFF,12+COFF
	FDB	5+COFF,13+COFF
	FDB	5+COFF,14+COFF
	FDB	5+COFF,15+COFF
	FDB	5+COFF,16+COFF
	FDB	5+COFF,16+COFF
	FDB	5+COFF,17+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100	1 EXTRA END OF POINTER ENTRY
*
*	VULTURE IMAGES
*		 (RIGHT)
*
*	BUZZARD RIGHT FACED IMAGES
*
*AAFF0A0E78B6FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUN1R	FDB	$0D0A
	IF UseCompiledSprites
    FDB BRUN1RCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$B6,$60,$00,$0F,$F8,$00
;	FCB	$00,$00,$06,$33,$31,$D0,$0F,$55,$50
;	FCB	$00,$00,$6B,$33,$3B,$1B,$0E,$F0,$50
;	FCB	$00,$0B,$0B,$BD,$B1,$1D,$08,$F0,$00
;	FCB	$00,$B0,$BB,$32,$BB,$1F,$FF,$80,$00
;	FCB	$0B,$6B,$B3,$33,$BD,$D8,$F8,$00,$00
;	FCB	$00,$0B,$3B,$BB,$88,$00,$00,$00,$00
;	FCB	$00,$BB,$00,$0F,$F0,$00,$00,$00,$00
;	FCB	$00,$00,$00,$8F,$FF,$00,$00,$00,$00
;	FCB	$00,$00,$00,$F8,$08,$80,$00,$00,$00
;	FCB	$00,$00,$00,$F0,$00,$F0,$00,$00,$00
;	FCB	$00,$00,$00,$F8,$00,$F0,$00,$00,$00
;	FCB	$00,$00,$00,$8F,$80,$00,$00,$00,$00
;	FCB	$00,$00,$00,$0E,$FF,$E0,$00,$00,$00
*AAFF090E78B6FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUN2R	FDB	$0D0A
	IF UseCompiledSprites
    FDB BRUN2RCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$0B    * Original values of sprite
  ENDIF
;	FCB	$36,$60,$00,$FF,$80,$00
;	FCB	$00,$00,$6B,$33,$31,$DE,$F5,$55,$00
;	FCB	$00,$06,$B3,$33,$BB,$DE,$EF,$05,$00
;	FCB	$00,$6B,$0B,$3D,$B1,$18,$8F,$00,$00
;	FCB	$00,$B0,$B3,$32,$3B,$FF,$FF,$00,$00
;	FCB	$03,$6B,$B3,$33,$BD,$8F,$FE,$00,$00
;	FCB	$00,$0B,$B3,$8B,$88,$00,$00,$00,$00
;	FCB	$00,$3B,$3B,$8F,$F0,$00,$00,$00,$00
;	FCB	$00,$BB,$00,$FF,$F0,$00,$00,$00,$00
;	FCB	$00,$00,$0F,$F0,$FF,$FF,$80,$00,$00
;	FCB	$00,$00,$0F,$00,$00,$0F,$F8,$00,$00
;	FCB	$00,$00,$0F,$80,$00,$80,$0F,$00,$00
;	FCB	$00,$00,$00,$F8,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$EF,$FE,$00,$00,$00,$00
*AAFF090E78B6FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUN3R	FDB	$0D0A
	IF UseCompiledSprites
    FDB BRUN3RCS         * Jump address to draw compiled sprite
    FCB Sprites02      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$0B    * Original values of sprite
  ENDIF
;	FCB	$36,$60,$00,$FF,$80,$00
;	FCB	$00,$00,$63,$33,$B1,$DE,$F5,$55,$00
;	FCB	$00,$06,$BB,$33,$3D,$DE,$EF,$05,$00
;	FCB	$00,$6B,$03,$3D,$B1,$18,$8F,$00,$00
;	FCB	$00,$B0,$BB,$23,$BC,$FF,$FF,$00,$00
;	FCB	$03,$BB,$B3,$3B,$BD,$8F,$FE,$00,$00
;	FCB	$00,$0B,$33,$8B,$88,$00,$00,$00,$00
;	FCB	$00,$3B,$3B,$88,$F8,$00,$00,$00,$00
;	FCB	$00,$BB,$0F,$FE,$FF,$E0,$00,$00,$00
;	FCB	$00,$00,$8F,$E0,$0F,$F0,$00,$00,$00
;	FCB	$00,$00,$FE,$00,$00,$8F,$00,$00,$00
;	FCB	$00,$00,$FE,$00,$00,$08,$F0,$00,$00
;	FCB	$00,$00,$8F,$80,$00,$0E,$FF,$00,$00
;	FCB	$00,$00,$EF,$F8,$00,$08,$08,$F0,$00
*AAFF0A0E78B6FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUN4R	FDB	$0D0A
	IF UseCompiledSprites
    FDB BRUN4RCS         * Jump address to draw compiled sprite
    FCB Sprites02        * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB EnemyBuzTransRight-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB BLK_ED_Player_Created_In_Transporter2  * Block where this sprite code is location
    FCB 0               * Move this many bytes forward in the Jump table for coloured sprite (Enemies are always white so ignored)
  ELSE
;	  FCB	$00,$00,$0B,$36,$60,$00,$0F       * Original values of sprite
  ENDIF
;  FCB $F8,$00
;	FCB	$00,$00,$63,$33,$31,$D0,$0F,$55,$50
;	FCB	$00,$06,$B3,$33,$3B,$1C,$0E,$F0,$50
;	FCB	$00,$6B,$03,$3D,$B1,$1F,$08,$F0,$00
;	FCB	$00,$B0,$BB,$32,$BB,$1F,$FF,$80,$00
;	FCB	$03,$BB,$BB,$3B,$BD,$D8,$F8,$00,$00
;	FCB	$00,$0B,$33,$8E,$88,$00,$00,$00,$00
;	FCB	$03,$3B,$EF,$88,$FE,$00,$00,$00,$00
;	FCB	$0B,$BE,$F8,$EF,$80,$00,$00,$00,$00
;	FCB	$00,$0F,$80,$0F,$E0,$00,$00,$00,$00
;	FCB	$00,$00,$F0,$08,$F0,$00,$00,$00,$00
;	FCB	$00,$00,$FF,$00,$0F,$00,$00,$00,$00
;	FCB	$00,$0F,$00,$F0,$EF,$FE,$00,$00,$00
;	FCB	$00,$00,$00,$00,$80,$FF,$00,$00,$00
*AAFF090D78B4FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUNSR	FDB	$0D09
	IF UseCompiledSprites
    FDB BRUNSRCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$0B,$22    * Original values of sprite
  ENDIF
;	FCB	$36,$00,$0F,$F8,$00,$00
;	FCB	$00,$32,$33,$BB,$B0,$0F,$55,$50,$00
;	FCB	$00,$B0,$30,$66,$6B,$08,$F0,$50,$00
;	FCB	$00,$00,$0B,$66,$31,$D0,$88,$00,$00
;	FCB	$00,$BB,$BB,$B3,$3B,$1E,$8F,$00,$00
;	FCB	$03,$30,$BB,$BD,$31,$1F,$F8,$00,$00
;	FCB	$06,$3B,$B3,$32,$BD,$D8,$E0,$00,$00
;	FCB	$00,$00,$63,$38,$8D,$80,$00,$00,$00
;	FCB	$00,$00,$00,$E8,$FF,$E8,$00,$00,$00
;	FCB	$00,$00,$00,$00,$E8,$FE,$80,$00,$00
;	FCB	$00,$00,$00,$00,$00,$F8,$E8,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0F,$8E,$80,$00
;	FCB	$00,$00,$00,$00,$00,$E8,$EF,$E8,$00
*AAFF0A0878B5FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BFLY1R	FDB	$0D0C
	IF UseCompiledSprites
    FDB BFLY1RCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$06    * Original values of sprite
  ENDIF
;	FCB	$B3,$6B,$60,$00,$00,$00
;	FCB	$00,$00,$62,$63,$36,$B6,$8F,$80,$00
;	FCB	$00,$0B,$32,$36,$6B,$B6,$8F,$55,$00
;	FCB	$00,$63,$3B,$3B,$BB,$B3,$88,$05,$00
;	FCB	$00,$33,$33,$B6,$B3,$BB,$36,$00,$00
;	FCB	$03,$3B,$0B,$0B,$30,$33,$B3,$B0,$00
;	FCB	$00,$6B,$EE,$E0,$B3,$03,$30,$33,$00
;	FCB	$00,$00,$E8,$8E,$0B,$30,$23,$02,$30
*AAFF0A0D78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BFLY3R	FDB	$0D09
	IF UseCompiledSprites
    FDB BFLY3RCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$03,$03,$B0    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00,$00
;	FCB	$03,$20,$3B,$00,$00,$00,$00,$00,$00
;	FCB	$0B,$33,$BB,$00,$00,$00,$00,$00,$00
;	FCB	$02,$03,$3B,$B0,$00,$00,$00,$00,$00
;	FCB	$0B,$30,$BB,$66,$00,$00,$00,$00,$00
;	FCB	$03,$B3,$3B,$B6,$BE,$00,$00,$00,$00
;	FCB	$00,$2B,$33,$B6,$6B,$BE,$00,$00,$00
;	FCB	$00,$03,$3B,$BB,$BB,$DB,$E8,$F8,$00
;	FCB	$00,$00,$63,$3B,$BB,$BD,$88,$F5,$50
;	FCB	$00,$0B,$06,$36,$B0,$11,$FF,$E0,$50
;	FCB	$03,$B3,$B0,$EE,$BB,$0D,$8E,$00,$00
;	FCB	$0B,$B6,$E8,$8E,$EE,$8E,$00,$00,$00
;	FCB	$00,$60,$E8,$EE,$0E,$00,$80,$00,$00
*
*		 (LEFT)
*
*AAFF0A0E78B6FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUN1L	FDB	$0D0A
	IF UseCompiledSprites
    FDB BRUN1LCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$8F,$F0    * Original values of sprite
  ENDIF
;	FCB	$00,$06,$6B,$00,$00,$00
;	FCB	$05,$55,$F0,$0D,$13,$33,$60,$00,$00
;	FCB	$05,$0F,$E0,$B1,$B3,$33,$B6,$00,$00
;	FCB	$00,$0F,$80,$D1,$1B,$DB,$B0,$B0,$00
;	FCB	$00,$08,$FF,$F1,$BB,$23,$BB,$0B,$00
;	FCB	$00,$00,$8F,$8D,$DB,$33,$3B,$B6,$B0
;	FCB	$00,$00,$00,$00,$88,$BB,$B3,$B0,$00
;	FCB	$00,$00,$00,$00,$0F,$F0,$00,$BB,$00
;	FCB	$00,$00,$00,$00,$FF,$F8,$00,$00,$00
;	FCB	$00,$00,$00,$08,$80,$8F,$00,$00,$00
;	FCB	$00,$00,$00,$0F,$00,$0F,$00,$00,$00
;	FCB	$00,$00,$00,$0F,$00,$8F,$00,$00,$00
;	FCB	$00,$00,$00,$00,$08,$F8,$00,$00,$00
;	FCB	$00,$00,$00,$0E,$FF,$E0,$00,$00,$00
*AAFF090E78B6FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUN2L	FDB	$0C0A
	IF UseCompiledSprites
    FDB BRUN2LCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $08,$FF,$00    * Original values of sprite
  ENDIF
;	FCB	$06,$63,$B0,$00,$00
;	FCB	$55,$5F,$ED,$13,$33,$B6,$00,$00
;	FCB	$50,$FE,$ED,$BB,$33,$3B,$60,$00
;	FCB	$00,$F8,$81,$1B,$D3,$B0,$B6,$00
;	FCB	$00,$FF,$FF,$B3,$23,$3B,$0B,$00
;	FCB	$00,$EF,$F8,$DB,$33,$3B,$B6,$30
;	FCB	$00,$00,$00,$88,$B8,$3B,$B0,$00
;	FCB	$00,$00,$00,$0F,$F8,$B3,$B3,$00
;	FCB	$00,$00,$00,$0F,$FF,$00,$BB,$00
;	FCB	$00,$08,$FF,$FF,$0F,$F0,$00,$00
;	FCB	$00,$8F,$F0,$00,$00,$F0,$00,$00
;	FCB	$00,$F0,$08,$00,$08,$F0,$00,$00
;	FCB	$00,$00,$00,$00,$8F,$00,$00,$00
;	FCB	$00,$00,$00,$EF,$FE,$00,$00,$00
*AAFF090E78B6FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUN3L	FDB	$0C0A
	IF UseCompiledSprites
    FDB BRUN3LCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $08,$FF,$00    * Original values of sprite
  ENDIF
;	FCB	$06,$63,$B0,$00,$00
;	FCB	$55,$5F,$ED,$1B,$33,$36,$00,$00
;	FCB	$50,$FE,$ED,$D3,$33,$BB,$60,$00
;	FCB	$00,$F8,$81,$1B,$D3,$30,$B6,$00
;	FCB	$00,$FF,$FF,$CB,$32,$BB,$0B,$00
;	FCB	$00,$EF,$F8,$DB,$B3,$3B,$BB,$30
;	FCB	$00,$00,$00,$88,$B8,$33,$B0,$00
;	FCB	$00,$00,$00,$8F,$88,$B3,$B3,$00
;	FCB	$00,$00,$0E,$FF,$EF,$F0,$BB,$00
;	FCB	$00,$00,$0F,$F0,$0E,$F8,$00,$00
;	FCB	$00,$00,$F8,$00,$00,$EF,$00,$00
;	FCB	$00,$0F,$80,$00,$00,$EF,$00,$00
;	FCB	$00,$FF,$E0,$00,$08,$F8,$00,$00
;	FCB	$0F,$80,$80,$00,$8F,$FE,$00,$00
*AAFF0A0E78B6FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUN4L	FDB	$0D0A
	IF UseCompiledSprites
    FDB BRUN4LCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB EnemyBuzTransLeft-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB BLK_ED_Player_Created_In_Transporter2  * Block where this sprite code is location
    FCB 0               * Move this many bytes forward in the Jump table for coloured sprite (Enemies are always white so ignored)
  ELSE
;	  FCB $00,$8F,$F0,$00,$06,$63,$B0    * Original values of sprite
  ENDIF
;	FCB	$00,$00
;	FCB	$05,$55,$F0,$0D,$13,$33,$36,$00,$00
;	FCB	$05,$0F,$E0,$C1,$B3,$33,$3B,$60,$00
;	FCB	$00,$0F,$80,$F1,$1B,$D3,$30,$B6,$00
;	FCB	$00,$08,$FF,$F1,$BB,$23,$BB,$0B,$00
;	FCB	$00,$00,$8F,$8D,$DB,$B3,$BB,$BB,$30
;	FCB	$00,$00,$00,$00,$88,$E8,$33,$B0,$00
;	FCB	$00,$00,$00,$00,$EF,$88,$FE,$B3,$30
;	FCB	$00,$00,$00,$00,$08,$FE,$8F,$EB,$B0
;	FCB	$00,$00,$00,$00,$0E,$F0,$08,$F0,$00
;	FCB	$00,$00,$00,$00,$0F,$80,$0F,$00,$00
;	FCB	$00,$00,$00,$00,$F0,$00,$FF,$00,$00
;	FCB	$00,$00,$00,$EF,$FE,$0F,$00,$F0,$00
;	FCB	$00,$00,$00,$FF,$08,$00,$00,$00,$00
*AAFF090D78B4FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BRUNSL	FDB	$0C09
	IF UseCompiledSprites
    FDB BRUNSLCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$8F,$F0    * Original values of sprite
  ENDIF
;	FCB	$00,$63,$22,$B0,$00
;	FCB	$05,$55,$F0,$0B,$BB,$33,$23,$00
;	FCB	$05,$0F,$80,$B6,$66,$03,$0B,$00
;	FCB	$00,$88,$0D,$13,$66,$B0,$00,$00
;	FCB	$00,$F8,$E1,$B3,$3B,$BB,$BB,$00
;	FCB	$00,$8F,$F1,$13,$DB,$BB,$03,$30
;	FCB	$00,$0E,$8D,$DB,$23,$3B,$B3,$60
;	FCB	$00,$00,$08,$D8,$83,$36,$00,$00
;	FCB	$00,$00,$8E,$FF,$8E,$00,$00,$00
;	FCB	$00,$08,$EF,$8E,$00,$00,$00,$00
;	FCB	$00,$8E,$8F,$00,$00,$00,$00,$00
;	FCB	$08,$E8,$F0,$00,$00,$00,$00,$00
;	FCB	$8E,$FE,$8E,$00,$00,$00,$00,$00
*AAFF0A0878B5FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BFLY1L	FDB	$0D0C
	IF UseCompiledSprites
    FDB BFLY1LCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$06,$B6,$3B,$60,$00,$00
;	FCB	$00,$08,$F8,$6B,$63,$36,$26,$00,$00
;	FCB	$00,$55,$F8,$6B,$B6,$63,$23,$B0,$00
;	FCB	$00,$50,$88,$3B,$BB,$B3,$B3,$36,$00
;	FCB	$00,$00,$63,$BB,$3B,$6B,$33,$33,$00
;	FCB	$00,$0B,$3B,$33,$03,$B0,$B0,$B3,$30
;	FCB	$00,$33,$03,$30,$3B,$0E,$EE,$B6,$00
;	FCB	$03,$20,$32,$03,$B0,$E8,$8E,$00,$00
*AAFF0A0D78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E81490CD111FA40A67
BFLY3L	FDB	$0D09
	IF UseCompiledSprites
    FDB BFLY3LCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$0B,$30,$30
;	FCB	$00,$00,$00,$00,$00,$00,$B3,$02,$30
;	FCB	$00,$00,$00,$00,$00,$00,$BB,$33,$B0
;	FCB	$00,$00,$00,$00,$00,$0B,$B3,$30,$20
;	FCB	$00,$00,$00,$00,$00,$66,$BB,$03,$B0
;	FCB	$00,$00,$00,$00,$EB,$6B,$B3,$3B,$30
;	FCB	$00,$00,$00,$EB,$B6,$6B,$33,$B2,$00
;	FCB	$00,$8F,$8E,$BD,$BB,$BB,$B3,$30,$00
;	FCB	$05,$5F,$88,$DB,$BB,$B3,$36,$00,$00
;	FCB	$05,$0E,$FF,$11,$0B,$63,$60,$B0,$00
;	FCB	$00,$00,$E8,$D0,$BB,$EE,$0B,$3B,$30
;	FCB	$00,$00,$00,$E8,$EE,$E8,$8E,$6B,$B0
;	FCB	$00,$00,$08,$00,$E0,$EE,$8E,$06,$00
*
*	PLAYER 1'S POINTERS
*
PLYR1	FDB	0,$02EF,PLY1R	RIDER ON SKIDDING HORSE
	FDB	0,$00EF,PLY1L
	FDB	0,$02ED,PLY1R	RIDER NORMALLY ON HORSE
	FDB	0,$00ED,PLY1L
*
*
*	RIDER FACEING RIGHT SIDE
*
PLY1R	FDB	$0303
	IF UseCompiledSprites
    FDB PLY1RCS         * Jump address to draw compiled sprite
    FCB Sprites03       * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB Ply1TransRWhite-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB Sprites05       * Block where this sprite code is location
    FCB Ply1TransRYellow-Ply1TransRWhite       * Move this many bytes forward in the Jump table for coloured sprite
  ELSE
;	  FCB $00,$55,$50    * Original values of sprite
;  	FCB	$00,$00,$00,$00
  ENDIF
;	FCB	$00,$57,$70,$00,$00,$00,$00
;	FCB	$00,$55,$00,$00,$00,$00,$00
;	FCB	$05,$55,$55,$00,$00,$00,$00
;	FCB	$05,$55,$55,$75,$55,$55,$50
;	FCB	$05,$55,$55,$00,$00,$00,$00
;	FCB	$00,$55,$55,$50,$00,$00,$00
*
*	RIDER FACEING LEFT SIDE
*
PLY1L	FDB	$0303
	IF UseCompiledSprites
    FDB PLY1LCS         * Jump address to draw compiled sprite
    FCB Sprites03       * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB Ply1TransLWhite-2 * Table address for a transporter sprite
    FCB Sprites05       * Block where this sprite code is location
    FCB Ply1TransLYellow-Ply1TransLWhite       * Move this many bytes forward in the Jump table for coloured sprite
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
;	  FCB	$00,$05,$55,$00
  ENDIF
;	FCB	$00,$00,$00,$00,$07,$75,$00
;	FCB	$00,$00,$00,$00,$00,$55,$00
;	FCB	$00,$00,$00,$00,$55,$55,$50
;	FCB	$05,$55,$55,$57,$55,$55,$50
;	FCB	$00,$00,$00,$00,$55,$55,$50
;	FCB	$00,$00,$00,$05,$55,$55,$00
*
*	PLAYER 2'S POINTERS
*
PLYR2	FDB	0,$02EF,PLY2R	RIDER ON SKIDDING HORSE
	FDB	0,$00EF,PLY2L
	FDB	0,$02ED,PLY2R	RIDER NORMALLY ON HORSE
	FDB	0,$00ED,PLY2L
*
*	RIDER FACEING RIGHT
*
PLY2R	FDB	$0303
	IF UseCompiledSprites
    FDB PLY2RCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB Ply2TransRWhite-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB Sprites06       * Block where this sprite code is location
    FCB Ply2TransRBlue-Ply2TransRWhite       * Move this many bytes forward in the Jump table for coloured sprite
  ELSE
;	  FCB $00,$77,$70,$00,$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$75,$50,$00,$00,$00,$00
;	FCB	$00,$77,$00,$00,$00,$00,$00
;	FCB	$07,$77,$77,$00,$00,$00,$00
;	FCB	$07,$77,$77,$57,$77,$77,$70
;	FCB	$07,$77,$77,$00,$00,$00,$00
;	FCB	$00,$77,$77,$70,$00,$00,$00
*
*	RIDER FACEING LEFT SIDE
*
PLY2L	FDB	$0303
	IF UseCompiledSprites
    FDB PLY2LCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB Ply2TransLWhite-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB Sprites06       * Block where this sprite code is location
    FCB Ply2TransLBlue-Ply2TransLWhite       * Move this many bytes forward in the Jump table for coloured sprite
  ELSE
;	  FCB $00,$00,$00,$00,$07,$77,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$05,$57,$00
;	FCB	$00,$00,$00,$00,$00,$77,$00
;	FCB	$00,$00,$00,$00,$77,$77,$70
;	FCB	$07,$77,$77,$75,$77,$77,$70
;	FCB	$00,$00,$00,$00,$77,$77,$70
;	FCB	$00,$00,$00,$07,$77,$77,$00
*
*
*	PL7YER 3'S POINTERS
*
PLYR3	FDB	0,$02EF,PLY3R	RIDER ON SKIDDING HORSE
	FDB	0,$00EF,PLY3L
	FDB	0,$02ED,PLY3R	RIDER NORMALLY ON HORSE
	FDB	0,$00ED,PLY3L
	FDB	CEGGMN,$00F5,PLY3S
*
*	RIDER FACEING RIGHT SIDE
*
PLY3R	FDB	$0303
	IF UseCompiledSprites
    FDB PLY3RCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB EnemyPlyTransRight-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB BLK_ED_Player_Created_In_Transporter2  * Block where this sprite code is location
    FCB 0               * Move this many bytes forward in the Jump table for coloured sprite (Enemies are always white so ignored)
  ELSE
;	  FCB $00,$44,$40    * Original values of sprite
;  	FCB	$00,$00,$00,$00
  ENDIF
;	FCB	$00,$00,$00,$00
;	FCB	$00,$49,$90,$00,$00,$00,$00
;	FCB	$00,$44,$00,$00,$00,$00,$00
;	FCB	$04,$44,$44,$00,$00,$00,$00
;	FCB	$04,$41,$11,$11,$11,$11,$10
;	FCB	$04,$44,$44,$00,$00,$00,$00
;	FCB	$00,$44,$44,$40,$00,$00,$00
*
*	RIDER FACEING LEFT SIDE
*
PLY3L	FDB	$0303
	IF UseCompiledSprites
    FDB PLY3LCS         * Jump address to draw compiled sprite
    FCB Sprites03       * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB EnemyPlyTransLeft-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB BLK_ED_Player_Created_In_Transporter2  * Block where this sprite code is location
    FCB 0               * Move this many bytes forward in the Jump table for coloured sprite (Enemies are always white so ignored)
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
;    FCB	$00,$04,$44,$00
  ENDIF
;	FCB	$00,$00,$00,$00,$09,$94,$00
;	FCB	$00,$00,$00,$00,$00,$44,$00
;	FCB	$00,$00,$00,$00,$44,$44,$40
;	FCB	$01,$11,$11,$11,$11,$14,$40
;	FCB	$00,$00,$00,$00,$44,$44,$40
;	FCB	$00,$00,$00,$04,$44,$44,$00
*
*	RIDER STANDING (LEFT & RIGHT)
*
*AAFF050C78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
PLY3S	FDB	$0108
	IF UseCompiledSprites
    FDB PLY3SCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$01,$00
;	FCB	$00,$E4,$4E,$01,$00
;	FCB	$00,$49,$9E,$01,$00
;	FCB	$00,$E4,$4E,$01,$00
;	FCB	$04,$44,$40,$4D,$00
;	FCB	$4E,$E4,$40,$44,$00
;	FCB	$00,$E4,$46,$6D,$00
;	FCB	$00,$44,$4E,$61,$00
;	FCB	$00,$4E,$E4,$61,$00
;	FCB	$0E,$4E,$E4,$61,$00
;	FCB	$08,$80,$04,$81,$00
;	FCB	$88,$80,$E8,$81,$00
*
*
*	PLAYER 4'S POINTERS
*
PLYR4	FDB	0,$02EF,PLY4R	RIDER ON SKIDDING HORSE
	FDB	0,$00EF,PLY4L
	FDB	0,$02ED,PLY4R	RIDER NORMALLY ON HORSE
	FDB	0,$00ED,PLY4L
	FDB	CEGGMN,$00F5,PLY4S
*
*	RIDER FACEING RIGHT SIDE
*
PLY4R	FDB	$0303
	IF UseCompiledSprites
    FDB PLY4RCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB EnemyPlyTransRight-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB BLK_ED_Player_Created_In_Transporter2  * Block where this sprite code is location
    FCB 0               * Move this many bytes forward in the Jump table for coloured sprite (Enemies are always white so ignored)
  ELSE
;	  FCB $00,$DD,$D0    * Original values of sprite
;  	FCB	$00,$00,$00,$00
  ENDIF
;	FCB	$00,$D4,$40,$00,$00,$00,$00
;	FCB	$00,$DD,$00,$00,$00,$00,$00
;	FCB	$0D,$DD,$DD,$00,$00,$00,$00
;	FCB	$0D,$D1,$11,$11,$11,$11,$10
;	FCB	$0D,$DD,$DD,$00,$00,$00,$00
;	FCB	$00,$DD,$DD,$D0,$00,$00,$00
*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE
*
	FCC	'JOUST (C) 1982 WILLIAMS ELECTRONICS INC.'
*
*	RIDER FACEING LEFT SIDE
*
PLY4L	FDB	$0303
	IF UseCompiledSprites
    FDB PLY4LCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB EnemyPlyTransLeft-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB BLK_ED_Player_Created_In_Transporter2  * Block where this sprite code is location
    FCB 0               * Move this many bytes forward in the Jump table for coloured sprite (Enemies are always white so ignored)
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
;  	FCB	$00,$0D,$DD,$00
  ENDIF
;	FCB	$00,$00,$00,$00,$04,$4D,$00
;	FCB	$00,$00,$00,$00,$00,$DD,$00
;	FCB	$00,$00,$00,$00,$DD,$DD,$D0
;	FCB	$01,$11,$11,$11,$11,$1D,$D0
;	FCB	$00,$00,$00,$00,$DD,$DD,$D0
;	FCB	$00,$00,$00,$0D,$DD,$DD,$00
*
*	PLAYER STANDING (LEFT & RIGHT)
*
*AAFF040C7DB0FFF4FFF4FFF4FFF4FFF4
*00FF704D0F3F41ED14904D111FA40A67
PLY4S	FDB	$0108
	IF UseCompiledSprites
    FDB PLY4SCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$01,$00
;	FCB	$00,$9D,$D9,$01,$00
;	FCB	$00,$D4,$4E,$01,$00
;	FCB	$00,$ED,$D6,$01,$00
;	FCB	$0D,$DD,$D9,$DA,$00
;	FCB	$D9,$6D,$D9,$DD,$00
;	FCB	$00,$6D,$D6,$6A,$00
;	FCB	$00,$DD,$DE,$61,$00
;	FCB	$00,$D9,$9D,$61,$00
;	FCB	$09,$DE,$ED,$61,$00
;	FCB	$03,$30,$0D,$31,$00
;	FCB	$33,$30,$93,$31,$00
*
*
*	PLAYER 5'S POINTERS
*
PLYR5	FDB	0,$02EF,PLY5R	RIDER ON SKIDDING HORSE
	FDB	0,$00EF,PLY5L
	FDB	0,$02ED,PLY5R	RIDER NORMALLY ON HORSE
	FDB	0,$00ED,PLY5L
	FDB	CEGGMN,$00F5,PLY5S
*
*	RIDER FACEING RIGHT SIDE
*
PLY5R	FDB	$0303
	IF UseCompiledSprites
    FDB PLY5RCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB EnemyPlyTransRight-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB BLK_ED_Player_Created_In_Transporter2  * Block where this sprite code is location
    FCB 0               * Move this many bytes forward in the Jump table for coloured sprite (Enemies are always white so ignored)
  ELSE
;	  FCB $00,$99,$90    * Original values of sprite
;  	FCB	$00,$00,$00,$00
  ENDIF
;	FCB	$00,$95,$50,$00,$00,$00,$00
;	FCB	$00,$99,$00,$00,$00,$00,$00
;	FCB	$09,$99,$99,$00,$00,$00,$00
;	FCB	$09,$91,$11,$11,$11,$11,$10
;	FCB	$09,$99,$99,$00,$00,$00,$00
;	FCB	$00,$99,$99,$90,$00,$00,$00
*
*	RIDER FACEING LEFT SIDE
*
PLY5L	FDB	$0303
	IF UseCompiledSprites
    FDB PLY5LCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
    FDB EnemyPlyTransLeft-2 * Table address for a transporter sprite - 2 because there is no size 0
    FCB BLK_ED_Player_Created_In_Transporter2  * Block where this sprite code is location
    FCB 0               * Move this many bytes forward in the Jump table for coloured sprite (Enemies are always white so ignored)
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
;    FCB	$00,$09,$99,$00
  ENDIF
;	FCB	$00,$00,$00,$00,$05,$59,$00
;	FCB	$00,$00,$00,$00,$00,$99,$00
;	FCB	$00,$00,$00,$00,$99,$99,$90
;	FCB	$01,$11,$11,$11,$11,$19,$90
;	FCB	$00,$00,$00,$00,$99,$99,$90
;	FCB	$00,$00,$00,$09,$99,$99,$00
*
*	RIDER STANDING (LEFT & RIGHT)
*AAFF050C78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
PLY5S	FDB	$0108
	IF UseCompiledSprites
    FDB PLY5SCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$01,$00
;	FCB	$00,$E9,$9E,$01,$00
;	FCB	$00,$95,$5E,$01,$00
;	FCB	$00,$E9,$9E,$01,$00
;	FCB	$09,$99,$90,$9D,$00
;	FCB	$9E,$E9,$90,$99,$00
;	FCB	$00,$E9,$96,$6D,$00
;	FCB	$00,$99,$9E,$61,$00
;	FCB	$00,$96,$69,$61,$00
;	FCB	$0E,$9E,$E9,$61,$00
;	FCB	$03,$30,$09,$31,$00
;	FCB	$33,$30,$E3,$31,$00
*
*	EGG STILLS & HATCHING
*
EGGI	FDB	CEGGUP,$00FA,EGGUP
	FDB	CEGGLF,$00FB,EGGLF
	FDB	CEGGRT,$00FB,EGGRT
	FDB	CEGGUP,$00FB,EGGB1
	FDB	CEGGMN,$FFF6,EGGB2
	FDB	CEGGMN,$FEF5,EGGB3
	FDB	CEGGMN,$00F5,PLY4S
*
CEGGUP
	FDB	3+COFF,6+COFF
	FDB	2+COFF,7+COFF
	FDB	2+COFF,7+COFF
	FDB	2+COFF,7+COFF
	FDB	2+COFF,7+COFF
	FDB	2+COFF,7+COFF
	FDB	3+COFF,6+COFF
	FDB	$8100,$8100
CEGGLF
	FDB	$8000,$8000
	FDB	2+COFF,6+COFF
	FDB	1+COFF,7+COFF
	FDB	1+COFF,7+COFF
	FDB	1+COFF,7+COFF
	FDB	2+COFF,7+COFF
	FDB	3+COFF,6+COFF
	FDB	$8100,$8100
CEGGRT
	FDB	$8000,$8000
	FDB	3+COFF,6+COFF
	FDB	2+COFF,7+COFF
	FDB	1+COFF,7+COFF
	FDB	1+COFF,7+COFF
	FDB	1+COFF,6+COFF
	FDB	2+COFF,5+COFF
	FDB	$8100,$8100	ENDING TERMINATOR
CEGGMN
	FDB	3+COFF,6+COFF
	FDB	3+COFF,6+COFF
	FDB	3+COFF,6+COFF
	FDB	2+COFF,4+COFF
	FDB	1+COFF,4+COFF
	FDB	3+COFF,8+COFF
	FDB	3+COFF,8+COFF
	FDB	3+COFF,6+COFF
	FDB	3+COFF,6+COFF
	FDB	2+COFF,7+COFF
	FDB	1+COFF,8+COFF
	FDB	$8100,$8100	ENDING TERMINATOR
*
*
*	EGG FALLING SEQUENCE
*
*AAFF050778B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
EGGUP	FDB	$0003
	IF UseCompiledSprites
    FDB EGGUPCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$B2,$2B    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$0B,$21,$12,$B0
;	FCB	$02,$11,$11,$20
;	FCB	$02,$15,$15,$20
;	FCB	$02,$51,$55,$20
;	FCB	$03,$25,$52,$30
;	FCB	$00,$32,$23,$00
*AAFF050678B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
EGGRT	FDB	$0002
	IF UseCompiledSprites
    FDB EGGRTCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$25,$52    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$02,$51,$15,$B0
;	FCB	$32,$11,$11,$30
;	FCB	$35,$11,$52,$B0
;	FCB	$B2,$55,$23,$00
;	FCB	$0B,$33,$30,$00
*AAFF050678B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
EGGLF	FDB	$0002
	IF UseCompiledSprites
    FDB EGGLFCS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $02,$55,$20    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$B5,$11,$52,$00
;	FCB	$31,$11,$12,$30
;	FCB	$B2,$51,$15,$30
;	FCB	$03,$25,$52,$B0
;	FCB	$00,$33,$3B,$00
*AAFF060678B074F774F67476F4F67474
*00FF70580F3F51E814905D111FA40A67
EGGB1	FDB	$0102
	IF UseCompiledSprites
    FDB EGGB1CS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $02,$00,$63    * Original values of sprite
  ENDIF
;	FCB	$53,$00
;	FCB	$25,$36,$4E,$52,$30
;	FCB	$51,$5E,$63,$51,$30
;	FCB	$21,$52,$E2,$15,$30
;	FCB	$32,$12,$35,$22,$00
;	FCB	$0B,$22,$32,$3B,$00
*AAFF080B78B074F774F67476F4F67474
*00FF70580F3F51E814905D111FA40A67
EGGB2	FDB	$0C0F
	IF UseCompiledSprites
    FDB EGGB2CS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$00,$63    * Original values of sprite
  ENDIF
;	FCB	$36,$00,$00,$00,$00
;	FCB	$00,$00,$B6,$46,$00,$00,$00,$00
;	FCB	$55,$00,$B3,$3E,$00,$00,$00,$00
;	FCB	$32,$03,$33,$E0,$80,$00,$05,$00
;	FCB	$00,$B0,$E3,$B0,$00,$00,$23,$00
;	FCB	$00,$00,$0B,$B3,$35,$30,$00,$00
;	FCB	$02,$30,$06,$6E,$31,$30,$00,$00
;	FCB	$01,$03,$36,$63,$23,$00,$00,$00
;	FCB	$01,$15,$36,$E0,$00,$00,$05,$00
;	FCB	$02,$11,$23,$E0,$21,$03,$11,$00
;	FCB	$00,$22,$22,$E2,$12,$33,$23,$00
*AAFF0A0C78B074F774F67476F4F67474
*00FF70580F3F51E814905D111FA40A67
EGGB3	FDB	$0E08
	IF UseCompiledSprites
    FDB EGGB3CS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB $00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$01,$00,$00,$00,$00
;	FCB	$15,$00,$00,$6D,$D6,$01,$00,$00,$00,$00
;	FCB	$32,$00,$00,$34,$43,$01,$00,$00,$05,$00
;	FCB	$00,$00,$00,$ED,$D3,$01,$00,$00,$52,$00
;	FCB	$00,$00,$03,$3D,$D0,$DD,$00,$00,$00,$00
;	FCB	$00,$00,$DE,$63,$D0,$2D,$00,$00,$00,$00
;	FCB	$00,$00,$00,$63,$36,$6D,$00,$00,$00,$00
;	FCB	$05,$00,$00,$32,$3E,$61,$00,$00,$00,$00
;	FCB	$03,$20,$00,$D6,$63,$61,$00,$00,$01,$00
;	FCB	$00,$00,$03,$3E,$E2,$61,$00,$00,$12,$00
;	FCB	$00,$00,$03,$30,$03,$31,$00,$00,$00,$00
;	FCB	$00,$00,$33,$30,$E3,$31,$00,$00,$00,$00
*
*	LAVA TROLL GRABBING HAND
*
ILAVAT	FDB	0,$00FB,GRAB1
	FDB	0,$00F8,GRAB2
	FDB	0,$00F8,GRAB3
	FDB	0,$00F2,GRAB4
	FDB	0,$00F0,GRAB5
	FDB	0,$00EF,GRAB6

*
*	LAVA TROOL HAND SEQUENCES
*
*AAFF040678B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
GRAB1	FDB	$0702
	IF UseCompiledSprites
    FDB GRAB1CS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$44,$8E,$00    * Original values of sprite
  ENDIF
;	FCB	$0E,$44,$E0
;	FCB	$00,$04,$80
;	FCB	$00,$0E,$40
;	FCB	$00,$00,$40
;	FCB	$00,$00,$40
*AAFF040978B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
GRAB2	FDB	$000D
	IF UseCompiledSprites
    FDB GRAB2CS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$0C    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$00,$00,$E4,$00
;	FCB	$00,$00,$44,$00
;	FCB	$00,$0E,$44,$00
;	FCB	$0E,$44,$4E,$00
;	FCB	$E4,$44,$E0,$00
;	FCB	$44,$E0,$00,$00
;	FCB	$48,$00,$00,$00
;	FCB	$4E,$00,$00,$00
*AAFF070978B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
GRAB3	FDB	$020D
	IF UseCompiledSprites
    FDB GRAB3CS         * Jump address to draw compiled sprite
    FCB Sprites03      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$40,$04,$00
;	FCB	$00,$04,$00,$40,$04,$E0
;	FCB	$00,$04,$0E,$40,$E4,$E0
;	FCB	$00,$04,$4C,$C4,$44,$00
;	FCB	$00,$04,$4A,$CC,$4E,$00
;	FCB	$00,$E4,$AA,$44,$E0,$00
;	FCB	$00,$4A,$A4,$E0,$00,$00
;	FCB	$04,$A8,$E0,$00,$00,$00
;	FCB	$4E,$00,$00,$00,$00,$00
*AAFF070F78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
GRAB4	FDB	$020B
	IF UseCompiledSprites
    FDB GRAB4CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$A0    * Original values of sprite
  ENDIF
;	FCB	$00,$80,$00
;	FCB	$00,$00,$A0,$0E,$A0,$00
;	FCB	$08,$00,$A8,$0A,$80,$00
;	FCB	$EA,$0E,$A8,$8A,$80,$A0
;	FCB	$AA,$08,$AA,$AA,$0A,$A0
;	FCB	$CA,$0A,$AA,$A8,$8A,$80
;	FCB	$8C,$AA,$AA,$AA,$AC,$00
;	FCB	$EC,$AA,$AA,$AC,$4E,$00
;	FCB	$08,$CA,$CA,$A4,$E0,$00
;	FCB	$0E,$4C,$AC,$4E,$00,$00
;	FCB	$00,$4C,$C4,$E0,$00,$00
;	FCB	$00,$44,$4E,$00,$00,$00
;	FCB	$0E,$44,$E0,$00,$00,$00
;	FCB	$E4,$4E,$00,$00,$00,$00
;	FCB	$4E,$00,$00,$00,$00,$00
*AAFF081178B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
GRAB5	FDB	$0315
	IF UseCompiledSprites
    FDB GRAB5CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$80,$00,$00,$00
;	FCB	$00,$00,$0E,$80,$0A,$00,$00
;	FCB	$00,$00,$0E,$E0,$8E,$0E,$E0
;	FCB	$0A,$00,$08,$EE,$80,$08,$80
;	FCB	$E8,$00,$E8,$08,$80,$88,$00
;	FCB	$88,$00,$EA,$A8,$E8,$80,$00
;	FCB	$EA,$80,$88,$5F,$AA,$0E,$80
;	FCB	$0A,$AF,$FE,$8F,$5E,$88,$E0
;	FCB	$0E,$A8,$F8,$AA,$AA,$80,$00
;	FCB	$08,$AA,$8A,$AA,$E8,$00,$00
;	FCB	$0E,$88,$8F,$5F,$80,$00,$00
;	FCB	$00,$8A,$A5,$FA,$E0,$00,$00
;	FCB	$08,$8E,$8A,$8E,$00,$00,$00
;	FCB	$04,$AA,$8E,$E0,$00,$00,$00
;	FCB	$E4,$C4,$CE,$00,$00,$00,$00
;	FCB	$84,$48,$E0,$00,$00,$00,$00
;	FCB	$48,$E0,$00,$00,$00,$00,$00
*AAFF081278B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
GRAB6	FDB	$0316
	IF UseCompiledSprites
    FDB GRAB6CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$AA,$F0,$00
;	FCB	$00,$00,$00,$8A,$88,$E8,$80
;	FCB	$00,$00,$0E,$F8,$EA,$EA,$A0
;	FCB	$00,$00,$0F,$AE,$08,$88,$E0
;	FCB	$00,$00,$AF,$AE,$8A,$AF,$F0
;	FCB	$00,$0E,$AA,$EE,$EE,$08,$80
;	FCB	$00,$08,$88,$8E,$8A,$FF,$80
;	FCB	$00,$EA,$FE,$A8,$E0,$88,$00
;	FCB	$0E,$8F,$88,$FA,$0F,$08,$00
;	FCB	$08,$4A,$88,$8E,$81,$E0,$00
;	FCB	$04,$A4,$8E,$08,$F1,$E0,$00
;	FCB	$04,$A8,$E0,$0F,$8F,$E0,$00
;	FCB	$04,$CE,$00,$0F,$EF,$E0,$00
;	FCB	$08,$CE,$00,$00,$F8,$E0,$00
;	FCB	$0E,$48,$00,$00,$00,$00,$00
;	FCB	$00,$48,$00,$00,$00,$00,$00
;	FCB	$04,$4E,$00,$00,$00,$00,$00
;	FCB	$48,$E0,$00,$00,$00,$00,$00
*
*	LAVA FLAME
*
IFLAME	FDB	0,$00F2,FLAME1
	FDB	0,$00F1,FLAME2
	FDB	0,$00F1,FLAME3
	FDB	0,$00F5,FLAME4
*
*	FLAME SEQUENCES
*
*AAFF050F78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
FLAME1	FDB	$000B
	IF UseCompiledSprites
    FDB FLAME1CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$40    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$00,$0E,$44,$00
;	FCB	$00,$04,$C4,$00
;	FCB	$00,$04,$A8,$00
;	FCB	$00,$08,$50,$00
;	FCB	$00,$00,$50,$00
;	FCB	$00,$00,$00,$00
;	FCB	$00,$00,$00,$00
;	FCB	$0E,$80,$00,$00
;	FCB	$00,$4E,$00,$00
;	FCB	$00,$E4,$40,$00
;	FCB	$00,$E4,$C4,$00
;	FCB	$00,$8C,$5A,$E0
;	FCB	$0E,$CA,$55,$C0
;	FCB	$E4,$A5,$55,$A0
*AAFF041078B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
FLAME2	FDB	$0014
	IF UseCompiledSprites
    FDB FLAME2CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$0E,$4E    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$00,$0E,$44,$00
;	FCB	$00,$00,$4A,$00
;	FCB	$00,$00,$04,$00
;	FCB	$00,$00,$00,$00
;	FCB	$00,$00,$00,$00
;	FCB	$00,$00,$00,$00
;	FCB	$04,$EE,$00,$00
;	FCB	$0E,$48,$E0,$00
;	FCB	$0E,$44,$80,$00
;	FCB	$00,$44,$CE,$00
;	FCB	$00,$4C,$A8,$00
;	FCB	$00,$4A,$5C,$00
;	FCB	$0E,$C5,$5C,$00
;	FCB	$08,$A5,$5C,$00
;	FCB	$4A,$55,$5A,$00
*AAFF051078B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
FLAME3	FDB	$0114
	IF UseCompiledSprites
    FDB FLAME3CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$4E,$00
;	FCB	$00,$00,$00,$E4,$00
;	FCB	$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00
;	FCB	$00,$00,$40,$00,$00
;	FCB	$00,$04,$00,$40,$00
;	FCB	$0E,$44,$0E,$40,$00
;	FCB	$04,$4E,$0C,$40,$00
;	FCB	$04,$C0,$4A,$80,$00
;	FCB	$0C,$54,$C8,$E0,$00
;	FCB	$0A,$5A,$C4,$00,$00
;	FCB	$0A,$55,$C4,$00,$00
;	FCB	$05,$55,$A8,$00,$00
;	FCB	$05,$55,$A8,$00,$00
;	FCB	$4A,$55,$A8,$00,$00
*AAFF040C78B0FFF4FFF4FFF4FFF4FFF4
*00FF70580F3F51E814905D111FA40A67
FLAME4	FDB	$0008
	IF UseCompiledSprites
    FDB FLAME4CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$04    * Original values of sprite
  ENDIF
;	FCB	$00
;	FCB	$00,$04,$44,$00
;	FCB	$00,$4C,$48,$00
;	FCB	$0E,$CA,$40,$00
;	FCB	$04,$AE,$00,$00
;	FCB	$04,$00,$00,$00
;	FCB	$04,$00,$00,$00
;	FCB	$84,$80,$00,$00
;	FCB	$4C,$CE,$00,$00
;	FCB	$4A,$5C,$E0,$00
;	FCB	$45,$55,$A0,$00
;	FCB	$4A,$55,$AE,$00
*
*******************************************************************************
*	PLAYER DEATH POOF IMAGES
*	 FROM KEN LANTZ
*
POOF1	POSOFF	0,$01,$18,FL1
POOF2	POSOFF	0,$01,$1E,FL2
POOF3	POSOFF	0,$00,$23,FL3
*
FL1	FDB	$0701
	IF UseCompiledSprites
    FDB FL1CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$04,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$44,$40
;	FCB	$04,$41,$44
;	FCB	$00,$44,$40
;	FCB	$00,$04,$00
*
FL2	FDB	$010D
	IF UseCompiledSprites
    FDB FL2CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$70    * Original values of sprite
  ENDIF
;	FCB	$00,$00
;	FCB	$07,$00,$70,$07,$00
;	FCB	$00,$70,$00,$70,$00
;	FCB	$00,$05,$05,$00,$00
;	FCB	$77,$00,$00,$07,$70
;	FCB	$00,$05,$05,$00,$00
;	FCB	$00,$70,$00,$70,$00
;	FCB	$07,$00,$70,$07,$00
;	FCB	$00,$00,$70,$00,$00
*
FL3	FDB	$020F
	IF UseCompiledSprites
    FDB FL3CS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$07,$00    * Original values of sprite
  ENDIF
;	FCB	$07,$00,$00
;	FCB	$00,$00,$07,$00,$00,$00
;	FCB	$07,$00,$00,$00,$07,$00
;	FCB	$00,$07,$01,$07,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00
;	FCB	$70,$71,$00,$01,$70,$70
;	FCB	$00,$00,$00,$00,$00,$00
;	FCB	$00,$07,$01,$07,$00,$00
;	FCB	$07,$00,$00,$00,$07,$00
;	FCB	$00,$00,$07,$00,$00,$00
;	FCB	$00,$07,$00,$07,$00,$00
*
*	PTERODACTYL, FLYING ONLY IMAGES
*
IPTERO	POSOFF	PT1RC,1,11,PT1R
	POSOFF	PT1LC,1,11,PT1L
	POSOFF	PT2RC,1,07,PT2R
	POSOFF	PT2LC,0,07,PT2L
	POSOFF	PT3RC,0,10,PT3R
	POSOFF	PT3LC,0,10,PT3L
*
PT1RC	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	9+COFF,14+COFF
	FDB	9+COFF,22+COFF
	FDB	5+COFF,26+COFF
	FDB	3+COFF,24+COFF
	FDB	5+COFF,21+COFF
	FDB	8+COFF,15+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100		TERMINATING POINTER
*
PT2RC	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	20+COFF,24+COFF
	FDB	10+COFF,27+COFF
	FDB	7+COFF,25+COFF
	FDB	5+COFF,25+COFF
	FDB	7+COFF,26+COFF
	FDB	5+COFF,21+COFF
	FDB	3+COFF,7+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100		TERMINATING POINTER
*
PT3RC	FDB	$8000,$8000
	FDB	27+COFF,28+COFF
	FDB	23+COFF,27+COFF
	FDB	22+COFF,26+COFF
	FDB	20+COFF,25+COFF
	FDB	9+COFF,27+COFF
	FDB	6+COFF,25+COFF
	FDB	4+COFF,26+COFF
	FDB	6+COFF,21+COFF
	FDB	8+COFF,16+COFF
	FDB	5+COFF,12+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100		TERMINATING POINTER
*
PT1LC	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	15+COFF,20+COFF
	FDB	7+COFF,20+COFF
	FDB	3+COFF,24+COFF
	FDB	5+COFF,26+COFF
	FDB	8+COFF,24+COFF
	FDB	14+COFF,21+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100
	FDB	$8100,$8100		TERMINATING POINTER
*
PT2LC	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	$8000,$8000
	FDB	5+COFF,9+COFF
	FDB	2+COFF,19+COFF
	FDB	4+COFF,22+COFF
	FDB	4+COFF,24+COFF
	FDB	3+COFF,22+COFF
	FDB	12+COFF,24+COFF
	FDB	22+COFF,26+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100		TERMINATING POINTER
*
PT3LC	FDB	$8000,$8000
	FDB	1+COFF,2+COFF
	FDB	2+COFF,6+COFF
	FDB	3+COFF,7+COFF
	FDB	4+COFF,9+COFF
	FDB	2+COFF,20+COFF
	FDB	4+COFF,23+COFF
	FDB	3+COFF,25+COFF
	FDB	8+COFF,23+COFF
	FDB	13+COFF,21+COFF
	FDB	17+COFF,24+COFF
	FDB	$8100,$8100
	FDB	$8100,$8100		TERMINATING POINTER
*
*AAFF0E0E80C2FF00FF40FF00FF00FF00
*00FF70580F3F51E814905D111FA40A67
PT1R	FDB	$090E
	IF UseCompiledSprites
    FDB PT1RCS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$99,$99,$F6    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$09,$44,$F7,$90,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$44,$CF,$79,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$09,$44,$FF,$70,$00,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$94,$4F,$79,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$00,$00,$94,$CF,$70,$00,$0D,$1F,$FD,$00,$00,$00
;	FCB	$00,$99,$44,$88,$F4,$14,$90,$00,$04,$00,$D1,$1F,$00
;	FCB	$99,$64,$8C,$FF,$CC,$C5,$FF,$CA,$88,$FF,$DD,$00,$00
;	FCB	$00,$99,$44,$CD,$FF,$88,$4F,$FC,$34,$E0,$00,$00,$00
;	FCB	$00,$00,$09,$44,$48,$86,$E0,$00,$00,$00,$00,$00,$00
*
*AAFF0E077FCBFF00FF40FF00FF00FF00
*00FF70580F3F51E814905D111FA40A67
*
PT2R	FDB	$0903
	IF UseCompiledSprites
    FDB PT2RCS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00,$0D,$1F,$FD,$00,$00
;	FCB	$00,$00,$00,$06,$36,$9F,$F1,$90,$00,$04,$00,$11,$10
;	FCB	$00,$00,$69,$94,$9F,$FC,$F9,$A0,$00,$EC,$8F,$80,$00
;	FCB	$00,$69,$4E,$94,$FC,$D8,$E8,$9C,$9F,$FD,$C9,$F0,$00
;	FCB	$00,$00,$94,$FC,$99,$6E,$98,$AD,$CF,$8E,$00,$ED,$00
;	FCB	$00,$94,$DD,$66,$0E,$E6,$88,$E0,$00,$00,$00,$00,$00
;	FCB	$94,$F4,$90,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
*
*AAFF0F0B7FC8FF00FF40FF00FF00FF00
*00FF70580F3F51E814905D111FA40A67
*
PT3R	FDB	$0B0F
	IF UseCompiledSprites
    FDB PT3RCS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8F,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FD,$ED,$10,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$DF,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$14,$F1,$C0,$00,$00
;	FCB	$00,$00,$00,$00,$63,$69,$86,$00,$00,$00,$0F,$DC,$E8,$40,$00
;	FCB	$00,$00,$06,$99,$AA,$AC,$FC,$A6,$00,$00,$CD,$43,$F0,$00,$00
;	FCB	$00,$06,$94,$84,$C3,$FF,$F1,$8A,$CD,$FF,$D8,$00,$ED,$00,$00
;	FCB	$00,$00,$06,$64,$6F,$FC,$F9,$8C,$DC,$F8,$E0,$00,$00,$00,$00
;	FCB	$00,$00,$00,$06,$4C,$78,$66,$8E,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$00,$99,$4C,$AA,$F7,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$99,$44,$CA,$AA,$FF,$D7,$D0,$00,$00,$00,$00,$00,$00,$00,$00
*
*AAFF0E0E80C2FF00FF40FF00FF00FF00
*00FF70580F3F51E814905D111FA40A67
PT1L	FDB	$090E
	IF UseCompiledSprites
    FDB PT1LCS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00,$00,$6F,$99,$99,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$09,$7F,$44,$90,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$97,$FC,$44,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$07,$FF,$44,$90,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$97,$F4,$49,$00,$00,$00,$00
;	FCB	$00,$00,$DF,$F1,$D0,$00,$07,$FC,$49,$00,$00,$00,$00
;	FCB	$F1,$1D,$00,$40,$00,$09,$41,$4F,$88,$44,$99,$00,$00
;	FCB	$00,$DD,$FF,$88,$AC,$FF,$5C,$CC,$FF,$C8,$46,$99,$00
;	FCB	$00,$00,$0E,$43,$CF,$F4,$88,$FF,$DC,$44,$99,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0E,$68,$84,$44,$90,$00,$00,$00
*
*AAFF0E077FCBFF00FF40FF00FF00FF00
*00FF70580F3F51E814905D111FA40A67
*
PT2L	FDB	$0A03
	IF UseCompiledSprites
    FDB PT2LCS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$00,$00,$DF    * Original values of sprite
  ENDIF
;	FCB	$F1,$D0,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$01,$11,$00,$40,$00,$09,$1F,$F9,$63,$60,$00,$00,$00,$00
;	FCB	$00,$08,$F8,$CE,$00,$0A,$9F,$CF,$F9,$49,$96,$00,$00,$00
;	FCB	$00,$0F,$9C,$DF,$F9,$C9,$8E,$8D,$CF,$49,$E4,$96,$00,$00
;	FCB	$00,$DE,$00,$E8,$FC,$DA,$89,$E6,$99,$CF,$49,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$0E,$88,$6E,$E0,$66,$DD,$49,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$4F,$49,$00
*
*AAFF0F0B7FC8FF00FF40FF00FF00FF00
*00FF70580F3F51E814905D111FA40A67
*
PT3L	FDB	$0B0F
	IF UseCompiledSprites
    FDB PT3LCS         * Jump address to draw compiled sprite
    FCB Sprites04      * Block needed to be loaded into Bank 6 - $C000-$DFFF
  ELSE
;	  FCB	$F8,$00,$00    * Original values of sprite
  ENDIF
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$01,$DE,$DF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$FD,$00,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$00,$0C,$1F,$41,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;	FCB	$04,$8E,$CD,$F0,$00,$00,$00,$68,$96,$36,$00,$00,$00,$00,$00
;	FCB	$00,$0F,$34,$DC,$00,$00,$6A,$CF,$CA,$AA,$99,$60,$00,$00,$00
;	FCB	$00,$DE,$00,$8D,$FF,$DC,$A8,$1F,$FF,$3C,$48,$49,$60,$00,$00
;	FCB	$00,$00,$00,$0E,$8F,$CD,$C8,$9F,$CF,$F6,$46,$60,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$E8,$66,$87,$C4,$60,$00,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$00,$7F,$AA,$C4,$99,$00,$00,$00
;	FCB	$00,$00,$00,$00,$00,$00,$00,$0D,$7D,$FF,$AA,$AC,$44,$99,$00
*
*	PTERODACTYL DISSOULVE
*
ASH1R
ASH1L
* PTERA1
	FCB	$D0,$1C,$01
	FCB	$B0,$2C,$14,$01
	FCB	$C0,$14,$01
	FCB	$B0,$14,$1C,$01
	FCB	$40,$34,$30,$24,$1C,$1C,$01
	FCB	$30,$14,$3C,$14,$20,$14,$2C,$01
	FCB	$20,$24,$2C,$34,$2C,$10,$14,$01
	FCB	$30,$34,$5C,$01
	FCB	$30,$24,$3C,$01
	FCB	$20,$2C,$01
	FCB	$24,$5C,$00
* PTERA2
	FCB	$D0,$14,$01
	FCB	$B0,$24,$1E,$01
	FCB	$C0,$2E,$01
	FCB	$B0,$1E,$14,$01
	FCB	$40,$3E,$30,$2E,$1E,$14,$01
	FCB	$30,$2E,$24,$1E,$20,$1E,$24,$01
	FCB	$20,$3E,$14,$2E,$10,$1E,$14,$2E,$01
	FCB	$30,$1E,$10,$2E,$24,$24,$01
	FCB	$30,$1E,$2E,$24,$01
	FCB	$20,$3E,$14,$01
	FCB	$3E,$24,$24,$00
* PTERA3
	FCB	$E0,$01
	FCB	$B0,$1E,$14,$1E,$01
	FCB	$C0,$2E,$01
	FCB	$B0,$1E,$1E,$01
	FCB	$40,$2E,$40,$2E,$1E,$1E,$01
	FCB	$30,$1E,$10,$1E,$14,$1E,$20,$1E,$14,$1E,$01
	FCB	$30,$2E,$10,$1E,$20,$1E,$10,$2E,$01
	FCB	$50,$2E,$1E,$14,$1E,$14,$01
	FCB	$40,$2E,$1E,$14,$01
	FCB	$20,$2E,$1E,$01
	FCB	$1E,$2E,$20,$1E,$14,$00
*
LENGTH_JOUSTI.SRC.OUT	EQU	*	CURRENT TOTAL LENGTH OF IMAGES
;	IFGT	*-$3B10
;	 FCB	$1111	OVERFLOW PAST $3B10
;	ENDIF
;	END	$E000

**** TB12REV3.SRC.out ****
******** NLIST
;	INCLUDE RAMDEF.SRC
;	INCLUDE SHRAMDEF.SRC
;	INCLUDE EQU.SRC
;	INCLUDE MESSEQU.SRC
;	INCLUDE MESSEQU2.SRC
;	* LIST


_.GODFLG	EQU	PPOSX+48-11	THE FLAG TO TELL IF THE ENTERIE WAS GOD
_.SCRSV	EQU	PPOSX+48-10	THE PEON SCORE AREA SAVE LOCATION
_.AREA	EQU	PPOSX+48-8	AREA FOR THE PEON TO ENTER AT
_.PLYNUM	EQU	PPOSX+48-6	THIS IS THE PLAYER NUMBER
_.SAVEA	EQU	PPOSX+48-5	WHERE TO STORE A
_.SAVEB	EQU	PPOSX+48-4	WHERE TO STORE B
_.SAVEX	EQU	PPOSX+48-3	WHERE TO STORE X
_.SAVEY	EQU	PPOSX+48-1	WHERE TO STORE Y


	ORG	TSBORG
	JMP	CKCMOS_TB12REV3
	JMP	OPTST_S_TB12REV3
	JMP	CMOSMV_TB12REV3
	JMP	ADVSC_TB12REV3
	JMP	CKHS_TB12REV3	CHECK HIGH SCORES (POWER UP)
	JMP	RESHSC_TB12REV3	RESET HIGH SCORES
	JMP	HSCHK_TB12REV3	CHECK HIGH SCORES AFTER FAC SET RESTORE
	JMP	HSBUT_TB12REV3	HIGH SCORE RESET BUTTON
	JMP	AUD_S_TB12REV3
	JMP	AUD1_S_TB12REV3
	JMP	SCCLER_TB12REV3
	JMP	RCMOSA_TB12REV3
	JMP	RCMOSB_TB12REV3
	JMP	RCMOSD_TB12REV3
	JMP	WCMOSA_TB12REV3
	JMP	WCMOSB_TB12REV3
	JMP	WCMOSD_TB12REV3
	JMP	HIGHPO_TB12REV3
  JMP	ACRED_TB12REV3
	JMP	COINL_TB12REV3
	JMP	COINC_TB12REV3
	JMP	COINR_TB12REV3
	JMP	DIVAB_TB12REV3
	JMP	BCDHEX_TB12REV3
	JMP	HEXBCD_TB12REV3
	JMP	ENTINT_TB12REV3
	JMP	OPOUT_TB12REV3
	JMP	ENDGAM_TB12REV3	END OF GAME HIGH SCORE PROCESSING

AMODE_TB12REV3:
	TST	ANYONE
	BEQ	2$
	PKILL	$00,$40		KILL EVERYONE EXCEPT H.S.T.D. AND START SWITCH
 	PCNAP	4
;	PSHS	CC
;	ORCC	#$FF  *  Using the Blitter to clear the screen here
;	LDD	#$9CD7
;	STD	$CA06	X & Y SIZE OF DMA
;	CLRA
;	CLRB
;	STD	$CA04	DESTINATION ADDRESS
;*	ORIGIN WE DON'T CARE ABOUT
;	STA	$CA01	WRITE CONSTANT OF '00'
;	LDA	#$12
;	STA	$CA00	START DMA FOR CONSTANT WRITE & NO ZERO SUPPRESS
;	PULS	CC
  JSR   ClearScreenLikeBlitter
	LDX	#$3920		--------|
	LDA	#MSBZB			|--GIVE THE CONGRATS
	LDB	#$11			|
	JSR	OUTPHR		--------|
	LDX	#$14CA
	LDA	#MSENT3
	LDB	#$99
	JSR	OUTP35
	LDX	VSNHIGH
	JSR	VSND
********	LDA	#$FF	(OLD TIME 255*20=5100TICKS = 1MIN 25 SEC)
********	STA	_.SAVEA,U
	CLR	_.SAVEA,U	(PFUTZ ALTERATION, WAIT FOR  2 MIN 9 SEC)
1$	PCNAP	30		 (OLD DATA 20)
	DEC	_.SAVEA,U
	BEQ	2$
	LDX	PLINK,U
	LDA	PID,X
	CMPA	#$41
	BEQ	1$
	CMPA	#$42
	BEQ	1$
3$	JMP	VATTRT
*
2$	PKILL	$42,$FF
	PCNAP	3
	BRA	3$
*
* CMOS MOVE: DESTINATION=Y, SOURCE=X, COUNT=B
*
CMOSMV_TB12REV3	PSHS	A	NEED A
CMMV1_TB12REV3	LDA	,X+	GET SOURCE
	EXG	X,Y	SWAP
	JSR	WCMOSA_TB12REV3	RITE IT
	EXG	X,Y	SWAP BACK
	DECB		DONE?
	BNE	CMMV1_TB12REV3	NO,LOOP
	PULS	A,PC	RETURN CLEAN
*
* CLEAR CMOS: CRASHES A,B,X
*
CLRALL_TB12REV3	LDX	#CMOS	POINT TO START
CLRA1_TB12REV3	CLR	,X+
	CMPX	#CMOS+$400
	BNE	CLRA1_TB12REV3	NO, LOOP
	RTS	BYE
*
* INIT CMOS: SAVES STATE
*
CMINI_S_TB12REV3	PSHS	X,Y,D	SAVE STATE
	LDX	#DEFALT_TB12REV3 POINT TO ROM
	LDY	#CMOS	POINT TO DESTINATION
	LDB	#DEFSIZ_TB12REV3 LENGTH
	BSR	CMOSMV_TB12REV3	MOVE THEM ALL
	PULS	X,Y,D,PC ALL DONE

OPSET_TB12REV3	PSHS	X,Y,D
	LDX	#OPD1_TB12REV3
	LDY	#OPMESS
	LDB	#52	50 BYTES WORTH
	BSR	CMOSMV_TB12REV3	DO THE XFER
	JSR	OPCHK_TB12REV3	FIND THE CHECK BYTE
	LDX	#OMESUM	PLACE TO STORE IT
	JSR	WCMOSA_TB12REV3 WRITE IT
	PULS	X,Y,D,PC
*
*
* DEFAULT HERE FOR NOW
*
DEFALT_TB12REV3	FCB	$20	REPLAY @20,000
	FCB	$05	NMEN
	FCB	$01	HIGH SCORE TO DATE ALLOWED
	FCB	$03	CSELCT
	FCB	$01	SLOT1M
	FCB	$01	SLOT2M
	FCB	$01	SLOT3M
	FCB	$01	CUNITC
	FCB	$00	CUNITB
	FCB	$00	MINUNT
	FCB	$05	GA1  MASTER DIFFICULTY
  IF DefaultLengthForGodsName
    FCB 32  Our max letters for God
  ELSE
	  FCB	$03	NUMBER OF LETTERS FOR GOD
  ENDIF
	FCB	$00	FACTORY SETTINGS
	FCB	$00	CLEAR AUDIT TOTALS
	FCB	$00	CLEAR HSTD
	FCB	$00	AUTOCYCLE
	FCB	$00	ENTER OPERATOR MESSAGE
	FCB	$00	ENTER HSTD
DEFSIZ_TB12REV3	EQU	*-DEFALT_TB12REV3
OPD1_TB12REV3
  FCB	CSPC,CSPC,CSPC,CSPC,CSPC,CSPC,CSPC
	FCB	CP,CR,CE,CS,CE,CN,CT,CE,CD,CSPC,CB,CY,CCOLN
	FCB	CSPC,CSPC,CSPC,CSPC,CSPC
OPD2_TB12REV3
  FCB	CW,CI,CL,CL,CI,CA,CM,CS,CSPC,CE,CL,CE,CC,CT,CR,CO,CN,CI,CC,CS,CSPC,CI,CN,CC,CPER
	FCB	$25,$29




ADVSC_TB12REV3	JSR	MAKCHK_TB12REV3	DO THE CHECKSUM
	JSR	SCCLER_TB12REV3
ADVSC2_TB12REV3	LDA	PIA0
	BITA	#2
	BNE	ADVSC2_TB12REV3	WAIT FOR RELEASE
ADVSC3_TB12REV3	LDA	SPECFN+3 CLEAR AUDITS??
	ANDA	#$F
	BEQ	ADVSC4_TB12REV3
	CLR	SPECFN+3
	JSR	MAKCHK_TB12REV3
	JSR	SCCLER_TB12REV3
	JSR	AUDCK4_TB12REV3	CLEAR THE AUDIT TOTALS AND PRINT THE MESSAGE.
	LDA	#$40
	JSR	NAPV
ADVSC4_TB12REV3	LDA	SPECFN+5 CHECK AUDIT MESSAGE
	ANDA	#$F
	BEQ	ADVSC6_TB12REV3	NOT THERE
	CLR	SPECFN+5
	BSR	MAKCHK_TB12REV3
	JSR	RSHSV	GO AND DO IT
	LDA	#$40
	JSR	NAPV
ADVSC6_TB12REV3	LDA	SPECFN+9 OPERATOR MESSAGE
	ANDA	#$F	LOOK AT RELEVANCE
	BEQ	ADVSC5_TB12REV3
	CLR	SPECFN+9
	BSR	MAKCHK_TB12REV3
	JSR	SCCLER_TB12REV3	CLEAR THE SCREEN
	LDA	#TXSETA
	JSR	TEXT	PRINT A PROMPTING MESSAGE
	JSR	OPENT_TB12REV3	GET THE OPERATOR MESSAGE
	LDA	$20
	JSR	NAPV
	JSR	OPCHK_TB12REV3	FORM THE CHECK BYTE
	LDX	#OMESUM	POINT
	JSR	WCMOSA_TB12REV3	AND STORE IT.
ADVSC5_TB12REV3	LDA	SPECFN+11 ENTER GOD'S NAME??
	ANDA	#$F
	BEQ	ADVSC8_TB12REV3
	CLR	SPECFN+11
	BSR	MAKCHK_TB12REV3
	JSR	SCCLER_TB12REV3	CLEAR THE SCREEN
	LDA	#TXSETG
	JSR	TEXT	PRINT A PROMPTING MESSAGE
	JSR	GODSET_TB12REV3	GO AND DO IT
	JSR	MKGODC_TB12REV3	MAKE GODS CHECK BYTE
ADVSC8_TB12REV3	LDA	SPECFN+7 AUTO CYCLE
	ANDA	#$F
	BEQ	NOAUT$
	CLR	SPECFN+7
	BSR	MAKCHK_TB12REV3
	BSR	FSCHK_TB12REV3
	JMP	AUTOCY
NOAUT$	BSR	FSCHK_TB12REV3
	BRA	CKCMOS_TB12REV3

FSCHK_TB12REV3	LDA	SPECFN+1
	ANDA	#$F	FACTORY SETTINGS??
	BEQ	ADVS33_TB12REV3
	INC	ADJSUM
	INC	ADJSUM	BASH THE CHECK BYTE...THIS WILL RESTORE.
	CLR	SPECFN+1 AND UNDO IT
ADVS33_TB12REV3	RTS


MAKCHK_TB12REV3	PSHS	X,A
	BSR	FCHK_TB12REV3	FIND THE NEW CHECKSUM.
	LDX	#ADJSUM
	JSR	WCMOSA_TB12REV3
	PULS	X,A,PC

FCHK_TB12REV3	PSHS	B,X,Y
	LDX	#CMOS
	LDY	#ENDADJ
	BSR	FCHK_S_TB12REV3
	PULS	B,X,Y,PC

OPCHK_TB12REV3	LDX	#OPMESS
	LDY	#OPMESS+104
*
FCHK_S_TB12REV3	STY	XTEMP
	CLRA
FCHK1_TB12REV3	LDB	,X+
	ANDB	#$F
	PSHS	B
	ADDA	,S+
	CMPX	XTEMP
	BNE	FCHK1_TB12REV3
	ADDA	#$37	FUDGE FACTOR
	RTS

CKADJ_TB12REV3	BSR	FCHK_TB12REV3
	PSHS	A
	LDX	#ADJSUM
	JSR	RCMOSA_TB12REV3
	CMPA	,S+
	RTS

CKCMOS_TB12REV3	BSR	OPTST_TB12REV3	CHECK OPERATOR MESSAGE
	BSR	CKADJ_TB12REV3	CHECK ADJ
	BEQ	CKSRT0_TB12REV3
	LDA	#WDATA
	STA	WDOG
	JSR	CMINI_S_TB12REV3
	LDA	#WDATA
	STA	WDOG
	BSR	MAKCHK_TB12REV3
	LDA	#WDATA
	STA	WDOG
	JSR	SCCLER_TB12REV3	CLEAR THE SCREEN
	LDA	#WDATA
	STA	WDOG
	BSR	AUDCHK_TB12REV3	CHECK FOR FAULTY AUDITS
	JSR	CHKHSV	CHECK FOR FAULTY HIGH SCORES
	JSR	CKHSV	NOW VALIDATE
	BSR	CKADJ_TB12REV3
	BEQ	CKSNOR_TB12REV3
	LDA	#TXIMES	TELL THE OPERATOR WHAT IS GOING ON.
CMLOP0_TB12REV3	JSR	TEXT
CMLOP_TB12REV3	LDA	#WDATA
	STA	WDOG
	LDA	PIA0	CHECK
	BITA	#2
	BEQ	CMLOP_TB12REV3	NOT PRESSED CONTINUE TO HANG
CKSMRT_TB12REV3	JMP	[$EFFE]	START HIM UP
CKSRT0_TB12REV3	JSR	CKHSV	FIX UP THE HIGH SCORE AREA AND RETURN
	BRA	CKSMRT_TB12REV3
CKSNOR_TB12REV3	LDA	#TXREST
	BRA	CMLOP0_TB12REV3

AUDCHK_TB12REV3	LDX	#SLOT1
	LDB	#4	FOUR BAD BYTES IN AUDIT AREA AFTER RESET IS ENOUGH.
AUDCK2_TB12REV3	LDA	,X+	GET A BYTE
	ANDA	#$F
	CMPA	#9	HEX???
	BLS	AUDCK1_TB12REV3	NOPE
	DECB		ONE MORE
	BEQ	AUDCK4_TB12REV3
AUDCK1_TB12REV3	CMPX	#ENDBOK+6 DONE??
	BNE	AUDCK2_TB12REV3
	RTS
AUDCK4_TB12REV3	LDA	#TXBKRS
	JSR	TEXT	PRINT IT
	LDX	#SLOT1
AUDCK5_TB12REV3	CLR	,X+	AND CLEAR EM.
	CMPX	#ENDBOK+6
	BNE	AUDCK5_TB12REV3
OPTRTS_TB12REV3	RTS

OPTST_TB12REV3	BSR	OPTST_S_TB12REV3	CHECK THE BYTE
	BEQ	OPTRTS_TB12REV3	ITS OK
	JMP	OPSET_TB12REV3	NO GOOD DEFAULT IT.


*
*	COMPARE FOR OPERATOR MESSAGE CHECK BYTE
*	BEQ FOR OK
*

OPTST_S_TB12REV3	JSR	OPCHK_TB12REV3	FORM CHECK BYTE
	PSHS	A	SAVE
	LDX	#OMESUM	GET THE SUM
	JSR	RCMOSA_TB12REV3
	CMPA	,S+
	RTS


*
HSBUT_TB12REV3	LDA	#$18	3 SECONDS REQUIRED
	STA	BCDD
	LDA	#$3F
	STA	SOUND
HSBUT1_TB12REV3	LDA	#$08
	JSR	NAPV	CHECK BUTTON
HSBUT2_TB12REV3	LDA	PIA0
	BITA	#8	STILL PRESSED??
	BEQ	HSBUT3_TB12REV3	NOPE....BYE
	DEC	BCDD
	BNE	HSBUT1_TB12REV3	NOT ENOUGH TIME YET
	LDY	#GODNAM	JUST PUT IN DEFAULT NAME
	LDX	#DEFHSR_TB12REV3
	LDB	#23	NOT THE SCORE, JUST THE NAMES
	JSR	CMSMOV
	JSR	MKGODC_TB12REV3	MAKE THE NEW CHECK BYTE
	CLR	SOUND	INDICATE MISSION ACCOMPLISHED
HSBUT3_TB12REV3	RTS

*
HSCHK_TB12REV3	LDY	#CMSCOR	START AT BEGINNING OF TABLE
	LDB	#8	8 BAD ENTRIES IS ADEQUATE FOR FULL BASH
HSCHK1_TB12REV3	JSR	FSCCK_TB12REV3	FORM THE CHECK BYTE
	EORA	6,Y	COMPARE TO CHECK BYTE THERE
	ANDA	#$0F
	BEQ	HSCHK2_TB12REV3
	DECB
	BEQ	RESHSC_TB12REV3	8 BAD ONES..RESET
HSCHK2_TB12REV3	LDA	#WDATA
	STA	WDOG
	LEAY	SCRSIZ,Y
	CMPY	#TODAYS	BEYOND REGION??
	BLO	HSCHK1_TB12REV3
	RTS
*
***	RESHSC_TB12REV3	 RESET HIGH SCORES
*
RESHSC_TB12REV3	LDA	#WDATA
	STA	WDOG
	LDX	#DEFHSR_TB12REV3	DEFAULT SCORES
	LDY	#GODNAM	GODS INITIALS FOLLOWED BY GODS SCORE FOLLOWED BY REST
	LDB	#CDEFS1_TB12REV3	SIZE OF DEFAULT TABLE
	JSR	CMSMOV	TRANSFER THE BLOCK
	LDX	#DEFSC2_TB12REV3
	LDY	#GODNAM+(2*CDEFS1_TB12REV3)
	LDB	#CDEFS2_TB12REV3
	JSR	CMSMOV
	JSR	MKGODC_TB12REV3
	LDY	#CMSCOR	WALK THROUGH
RESHS1_TB12REV3	JSR	MKSCCK_TB12REV3
	LDA	#WDATA
	STA	WDOG
	LEAY	SCRSIZ,Y
	CMPY	#TODAYS
	BLO	RESHS1_TB12REV3
	LDA	#TXRESM	SAY THAT THEY WERE RESET
	JMP	TEXT	AND RETURN

DEFHSR_TB12REV3
DEFGOD_TB12REV3	FCB	CJ,CO,CU,CS,CT,CSPC
	FCB	CW,CI,CL,CL,CI,CA,CM,CS
	FCB	CSPC,CSPC,CSPC,CSPC,CSPC,CSPC
	FCB	CW,CI,CL	#1
	FCB	$00,$10,$91,$02		09/10/82 NEW REV. TO HANDEL
*					 ATTRACT MODE (AT GAME ADJUST 0 TO 3)
*					 NOT ERASEING TEXT
***********	FCB	$00,$10,$72,$12		07/21/82
	FCB	CM,CR,CS	#2
	FCB	$00,$04,$84,$93
	FCB	CJ,CR,CN	#3
	FCB	$00,$04,$71,$13
	FCB	CP,CF,CZ	#4
	FCB	$00,$04,$61,$75
	FCB	CC,CW,CK	#5
	FCB	$00,$04,$52,$22
	FCB	CK,CF,CL	#6
	FCB	$00,$04,$42,$10
	FCB	CP,CG,CD	#7
	FCB	$00,$04,$32,$17
	FCB	CK,CE,CN	#8
	FCB	$00,$04,$29,$99
	FCB	CJ,CA,CN	#9
	FCB	$00,$04,$10,$11
	FCB	CC,CJ,CM	#10
	FCB	$00,$04,$05,$23
	FCB	CS,CJ,CM	#11
	FCB	$00,$03,$99,$09
	FCB	CC,CR,CB	#12
	FCB	$00,$03,$80,$01
	FCB	CP,CV,CA	#13
	FCB	$00,$03,$72,$10
	FCB	CG,CW,CW	#14
	FCB	$00,$03,$61,$91
	FCB	CR,CO,CN	#15
	FCB	$00,$03,$51,$01
CDEFS1_TB12REV3	EQU	*-DEFHSR_TB12REV3
DEFSC2_TB12REV3	FCB	CJ,CO,CE	#16
	FCB	$00,$03,$42,$11
	FCB	CT,CI,CM	#17
	FCB	$00,$03,$35,$67
	FCB	CE,CSPC,CA	#18
	FCB	$00,$03,$28,$90
	FCB	CJ,CI,CM	#19
	FCB	$00,$03,$19,$01
	FCB	CW,CE,CS	#20
	FCB	$00,$03,$01,$57
	FCB	CL,CE,CO	#21
	FCB	$00,$02,$92,$30
	FCB	CB,CU,CZ	#22
	FCB	$00,$02,$87,$77
	FCB	CJ,CJ,CK	#23
	FCB	$00,$02,$79,$87
	FCB	CS,CA,CK	#24
	FCB	$00,$02,$69,$59
	FCB	CD,CE,CB	#25
	FCB	$00,$02,$58,$88
	FCB	CN,CSPC,CF	#26
	FCB	$00,$02,$46,$75
TODTAB_TB12REV3	FCB	CJ,CR,CN	#27
	FCB	$00,$02,$33,$10
	FCB	CP,CF,CZ	#28
	FCB	$00,$02,$29,$17
	FCB	CK,CF,CL	#29
	FCB	$00,$02,$25,$52
	FCB	CC,CW,CK	#30
	FCB	$00,$02,$05,$22
	FCB	CJ,CA,CN	#31
	FCB	$00,$01,$76,$35
	FCB	CM,CR,CS	#32
	FCB	$00,$01,$65,$35
ENDTOD_TB12REV3	FCB	CK,CA,CY	#33
	FCB	$00,$01,$55,$05
	FCB	CJ,CG,CL	#34
	FCB	$00,$01,$43,$15
	FCB	CR,CA,CM	#35
	FCB	$00,$01,$31,$09
	FCB	CH,CE,CC	#36
	FCB	$00,$01,$20,$10
	FCB	CK,CV,CD	#37
	FCB	$00,$01,$17,$55
	FCB	CE,CJ,CS	#38
	FCB	$00,$01,$05,$02
	FCB	CV,CA,CX	#39
	FCB	$00,$00,$94,$05
	FCB	CD,CR,CJ	#40
	FCB	$00,$00,$83,$11
	FCB	CJ,CA,CY	#41	HE'S NEVER REALLY SEEN
	FCB	$00,$00,$70,$01
*
CDEFS2_TB12REV3	EQU	*-DEFSC2_TB12REV3	LENGTH OF TABLE
*
NULSCR_TB12REV3	FCB	CSPC,CSPC,CSPC
	FCB	$00,$00,$40,$00
NULSIZ_TB12REV3	EQU	*-NULSCR_TB12REV3


CLRSCR_TB12REV3	PSHS	X,Y,B	SAVE BASHED
	LDX	#NULSCR_TB12REV3	POINT AT NULL SCORE
	LDB	#NULSIZ_TB12REV3	HALF AN ENTRYS WORTH OF BYTES
	JSR	CMSMOV
	PULS	X,Y,B,PC

MKGODC_TB12REV3	PSHS	A
	BSR	FGODC_TB12REV3	FOR GODS CHECK BYTE	IN LOW HALF OF A
	STA	GODNAM+46 STORE AFTER THE CHARACTERS
	PULS	A,PC

*
**	FORM GOD'S CHECK BYTE
*

FGODC_TB12REV3	PSHS	X
	LDX	#GODNAM	LETS START AT THE VERY BEGINNING....
	CLRA
FGODC0_TB12REV3	ADDA	,X	ADD THE CURRENT NIBBLE
FGODC1_TB12REV3	LEAX	1,X
	CMPX	#GODNAM+46 CHECK BYTE???
	BEQ	FGODC1_TB12REV3
	CMPX	#GODNAM+54 DONE???
	BNE	FGODC0_TB12REV3
	PULS	X,PC	RETURN IN LOW HALF OF A

MKSCCK_TB12REV3	PSHS	A
	BSR	FSCCK_TB12REV3	FORM THE CHECK BYTE
	STA	6,Y	STORE IT
	PULS	A,PC

FSCCK_TB12REV3	PSHS	Y,B
	LDB	#SCRSIZ
	CLRA
MSCCK0_TB12REV3	CMPB	#SCRSIZ-6 ABOUT TO DO CHECK BYTE??
	BEQ	MSCCK1_TB12REV3
	ADDA	,Y	ADD THE CURRENT BYTE
MSCCK1_TB12REV3	LEAY	1,Y
	DECB
	BNE	MSCCK0_TB12REV3
	PULS	Y,B,PC

CKHS_TB12REV3	LDA	#50	DONT GO THROUGH MORE THAN 50
	PSHS	A
	LDY	#CMSCOR	WALK THROUGH SCORES
CKHS7_TB12REV3	BSR	FSCCK_TB12REV3	FORM THE SCORE CHECK BYTE
	EORA	6,Y
	ANDA	#$F
	BEQ	CKHS5_TB12REV3
CKHS4_TB12REV3	JSR	RMENTR_TB12REV3	REMOVE THE ENTRY
	CLR	CREDST
	CLR	CREDST+1
	DEC	,S	MAKE SURE WE DON'T REMOVE TOO MANY
	BEQ	CKHS67_TB12REV3
	BRA	CKHS7_TB12REV3	DON'T PUSH POINTER YET.
CKHS5_TB12REV3	LDA	#3
	LDB	#4
	BSR	CKENT_TB12REV3
	BCS	CKHS4_TB12REV3
CKHS6_TB12REV3	LEAY	SCRSIZ,Y MOVE TO NEXT
	CMPY	#TODAYS
	BLO	CKHS7_TB12REV3
CKHS67_TB12REV3	PULS	A	TAKE ENTRY OFF OF STACK
	LDX	#TODTAB_TB12REV3	TODAY'S TABLE (NORMAL BYTES)
	LDY	#TODAYS
	LDB	#ENDTOD_TB12REV3-TODTAB_TB12REV3
	JSR	CMSMOV
	BSR	FGODC_TB12REV3	NOW CHECK OUT GOD.
	EORA	GODSCR CHECK IT OUT
	ANDA	#$F
	BEQ	CKHS1_TB12REV3	OK..CONTINUE
	BSR	REMGOD_TB12REV3
CKHS1_TB12REV3	LDY	#GODNAM	BEGINNING OF TEXT ONLY PART-CHECK ALPHA NUM
	LDA	#23	20 CHARS OF TEXT
	LDB	#4	4 BYTES OF NUMBERS PLEASE
	BSR	CKENT_TB12REV3	MAKE SURE ALPHAS AND NUMERICS
	BCC	CKHS3_TB12REV3
	BSR	REMGOD_TB12REV3	REMOVE GOD
CKHS3_TB12REV3	RTS

REMGOD_TB12REV3	LDX	#GODNAM	GOD BAD FIRST BLANK OUT
	LDA	#CSPC
CKHS2$	JSR	WCMOSA_TB12REV3
	CMPX	#GODNAM+40	DONE??
	BLO	CKHS2$	DO ANOTHER CHARACTER
	LDX	#CMSCOR	#2
	LDY	#GODNAM
	LDA	#6	6 NIBBLES
	JSR	BLKMOV_TB12REV3	MOVED
	LDY	#GODINT	X STILL POINTS AT FIRST GUYS INITIALS
	BSR	BLKMOV_TB12REV3	MOV EM TOO
	LDX	#CMSCOR+6 #2 SCORE
	LDY	#GODSCR #2 SCORE GOES HERE
	LDA	#8
	BSR	BLKMOV_TB12REV3
	JSR	MKGODC_TB12REV3	MAKE THE CHECK BYTE
	LDY	#CMSCOR	NOW REMEOVE #2
	BRA	RMENTR_TB12REV3	REMOVE #2 AND RETURN

CKENT_TB12REV3	PSHS	D,X
	LDB	#WDATA
	STB	WDOG
	TFR	Y,X	CMOSABLE REGISTER
CKENT1_TB12REV3	JSR	RCMOSB_TB12REV3	READ A BYTE
	CMPB	#$0A	LOWER THAN A SPACE??
	BLO	CKENT5_TB12REV3	YEP...NOT VALID
CKENT4_TB12REV3	CMPB	#$24
	BHI	CKENT5_TB12REV3	HIGHER THAN Z
	DECA		ONE LESS ON THIS FRONT
	BNE	CKENT1_TB12REV3
	LDA	1,S	GET THE SECOND PART COUNTER BACK
	JSR	RCMOSB_TB12REV3	GET THE CHECK BYTE
	ANDB	#$F
	CMPB	#9
	BHI	CKENT5_TB12REV3	NO HEX ALLOWED
	DECA
CKENT2_TB12REV3	JSR	RCMOSB_TB12REV3
	PSHS	B
	ANDB	#$F	LOOK AT LOW HALF
	CMPB	#9
	PULS	B
	BHI	CKENT5_TB12REV3	NOT NUMERIC...ERROR!
	ANDB	#$F0
	CMPB	#$99
	BHI	CKENT5_TB12REV3
	DECA
	BNE	CKENT2_TB12REV3
CKENT3_TB12REV3	ANDCC	#$FE	(CLC)
CKENT8_TB12REV3	LDA	#WDATA
	STA	WDOG
	PULS	X,D,PC
CKENT5_TB12REV3	ORCC	#$01	SEC
	BRA	CKENT8_TB12REV3

*
***	RMENTR_TB12REV3 - REMOVE SCORE ENTRY POINTED TO BY Y.
*

RMENTR_TB12REV3	PSHS	X,Y,D
	LEAX	SCRSIZ,Y X POINTS PAST Y
RMENT0_TB12REV3	CMPX	#TODAYS	ARE WE BEYOND IN X.
	BHS	RMENT1_TB12REV3	YEP...DONE.
	LDA	#SCRSIZ
	BSR	BLKMOV_TB12REV3	MOVE THE BLOCK X TO Y
	LEAY	SCRSIZ,Y
	LEAX	SCRSIZ,X
	LDA	#WDATA
	STA	WDOG
	BRA	RMENT0_TB12REV3
RMENT1_TB12REV3	JSR	CLRSCR_TB12REV3	CLEAR THE BLOCK NOW POINTED AT BY Y (BOTTOM)
	JSR	MKSCCK_TB12REV3	AND FORM THE CHECK BYTE
	PULS	X,Y,D,PC

*	BLKMOV_TB12REV3	A BYTES FROM [X] TO [Y]

BLKMOV_TB12REV3	PSHS	X,Y,D
BLKMV1_TB12REV3	LDB	,X+
	STB	,Y+
	DECA
	BNE	BLKMV1_TB12REV3
	PULS	X,Y,D,PC
*
*	WARNING, DMA SCREEN CLEARS DROP INTERUPTS (AND COINS)
*
********SCCLER_TB12REV3	PSHS	D,X,CC		***PFUT, NEED INTERUPTS ALL THE TIME
********	ORCC	#$FF
********	LDX	#$0000
********	LDD	#$9C84
********	STX	$CA04
********	STD	$CA06
********	CLRB
********	STB	$CA01
********	LDB	#$12
********	LDA	#WDATA
********	STA	WDOG
********	STB	$CA00
********	LDX	#$0080
********	STX	$CA04
********	LDB	#$12
********	LDA	#WDATA
********	STA	WDOG
********	STB	$CA00
********	LDA	#WDATA
********	STA	WDOG
********	PULS	D,X,CC,PC
*

* Clear the screen - moved jump to my own code in hi memory $F000- area
;SCCLER_TB12REV3:
	PSHS	D,X,Y,U
	LDD	#$0000
	TFR	D,X
	TFR	D,Y
	LDU	#$A000		START SCREEN ADDRESS
	LDA	#WDATA
10$	PSHU	B,X,Y		30 CLEARED BYTES
	PSHU	B,X,Y
	PSHU	B,X,Y
	PSHU	B,X,Y
	PSHU	B,X,Y
	PSHU	B,X,Y
	STA	WDOG
	CMPU	#-20	   $A000-((($A000+30-1)/30)*30) COMPLICATED END POINT
	BNE	10$		BR=DO NOT STOP
	PULS	D,X,Y,U,PC
*
* CMOS PRIMITIVE FOR READING   $40E1
*
* Moved this routine to hi RAM $F000 - area
;RCMOSA_TB12REV3:	EQU	*
;RCMOS_TB12REV3:

*	LDA	1,X	GET LSB						*** Commented out a few lines to make CMOS routines take the same amount of space as before
*	ANDA	#$0F	LEAVE ONLY LS
	PSHS	A	SAVE IT
	LDA	,X++	GET MSB + AUTO INC
*	ASLA
	ASLA
	ASLA
	ASLA		SHIFT LS TO MSB
	ADDA	,S+	GET LSB + FIX STACK
DRTS_TB12REV3	RTS		BYE
*
* READ CMOS INTO D POINTED TO BY X: A=X,X+1; B=X+2,X+3
*
RCMOSD_TB12REV3:
	JSR	RCMOSA_TB12REV3	GET THE FIRST BYTE+FALL THRU FOR 2ND  * was a BSR
*
* READ CMOS INTO B POINTED TO BY X
*
RCMOSB_TB12REV3:
	PSHS	A	SAVE A
	JSR	RCMOSA_TB12REV3	GET IN A  														* was a BSR
	TFR	A,B	PUT IT IN B
	PULS	A,PC	DONE
*
* WRITE TO CMOS PRIMITIVE
*
* Moved this routine to hi RAM $F000 - area
;WCMOSA_TB12REV3:	EQU	*
;WCMOS_TB12REV3:
	PSHS	A	SAVE WHATS TO BE WRITTEN
	STA	1,X	SAVE LSB
	LSRA	SHIFT	MS TO LS
	LSRA
	LSRA
	LSRA
	STA	,X++	SAVE MSB AND AUTO INC
	PULS	A,PC	DONE
*
* WRITE CMOS FROM D TO X: A=X,X+1; B=X+2,X+3
*
WCMOSD_TB12REV3:
	JSR	WCMOSA_TB12REV3	DO IT AND FALL THRU FOR 2ND						* was a BSR
*
* WRITE CMOS FROM B TO X
*
WCMOSB_TB12REV3:
	PSHS	A	SAVE A
	TFR	B,A	MOVE B TO A
	JSR	WCMOSA_TB12REV3	FAKE IT																* was a BSR
	PULS	A,PC	DONE

*
*
* AUDIT: COUNT=A, COUNTER=B (1-7)
*
AUD1_S_TB12REV3	PSHS	D,X	SAVE STATE
	LDA	#$01	BUMP BY COUNT OF 1
	BRA	AUD2_TB12REV3
AUD_S_TB12REV3	PSHS	D,X	SAVE STATE
AUD2_TB12REV3	ANDB	#$0F	LIMIT TO 0-F
	ASLB	MAKE	INTO 4X
	PSHS	B
	ASLB
	ADDB	,S+	6X
	LDX	#CMOS-4+$100 POINT TO START-6	(CREDITS TAKES UP 2)
	ABX		GET THE CORRECT DESTINATION
	BSR	RCMOSB_TB12REV3
	PSHS	B
	BSR	RCMOSB_TB12REV3
	PSHS	B
	BSR	RCMOSB_TB12REV3
	PSHS	B
	ADDA	,S
	DAA
	STA	,S
	LDA	1,S
	ADCA	#0
	DAA
	STA	1,S
	LDA	2,S
	ADCA	#0
	DAA
	LEAX	-6,X
	JSR	WCMOSA_TB12REV3												* Changed from a BSR
	PULS	B
	PULS	A
	BSR	WCMOSD_TB12REV3
	PULS	A
AUDX_TB12REV3	PULS	D,X,PC	CLEAN UP				* $414D
*
* Draw High scores screen
HIGHPO_TB12REV3:
	JSR	VTEST

	LDA	#TXHSP	ROUTINE TO PUT OUT HIGH SCORE TABLE PAGE
	JSR	TEXT
	CLR	XTEMP
	LDX	#$1170
	LDA	#$F1	BLANK THE 0 IN NUMBER ONE
	LDB	#$22	COLOR 2
	LDY	#GODNAM
	CMPY	HSPP1
	BNE	20$
	LDB	#RED
	BRA	22$
20$	CMPY	HSPP12
	BNE	22$
	LDB	#GREEN
22$	JSR	OUTBCD
	LDA	#CBRKR
	JSR	OUTCHR
	LEAX	$300,X	IMMITATE A SPACE
	LDA	#20+1
	STA	BCDR
1$	DEC	BCDR
	BEQ	10$
	EXG	X,Y	X=CMOS, Y=DEST
	JSR	RCMSA
	CMPA	#CSPC	IS IT A SPACE
	BGT	2$
	TST	XTEMP
	BNE	3$
	STY	XTEMP
	BRA	3$
2$	CLR	XTEMP
3$	EXG	X,Y	X=DEST, Y=CMOS
	JSR	OUTCHR
	BRA	1$
10$	TST	XTEMP
	BEQ	12$
	LDX	XTEMP
12$	LDA	#$04
	STA	BCDR
	LDY	#GODSCR
	EXG	X,Y	X=CMOS, Y=DEST
	JSR	RCMSA
	EXG	X,Y	X=DEST, Y=CMOS
	ORA	#$F0
	BITA	#$0F
	BNE	13$
	ORA	#$0F
13$	JSR	OUTBCD
	DEC	BCDR
11$	EXG	X,Y	X=CMOS, Y=DEST
	JSR	RCMSA
	EXG	X,Y	X=DEST, Y=CMOS
	JSR	OUTBCD
	DEC	BCDR
	BNE	11$

*	TIME TO DO THE SMALL STUFF

	LDX	#$1380	STARTING COLUMN
	LDB	#$33	COLOR 3
	LDU	#OUTB35
	STU	BCDN
	LDU	#OUTC35
	LDA	#13
	STA	BCDR	WE WANT THIRTEEN NAMES IN A COLUMN
	LDA	#$2
	STA	BCDD	WERE STARTING AT NUMBER TWO IN THE TABLE
	LDA	#$7
	STA	BCDN+2	WE WANT SEVEN LINES BETWEEN THE TOPS
	LDA	#16						* Changed for 14 to 16 (give a little more space for wider initials)
	STA	BCDTMP+1 WE WANT IT 14 OVER FROM THE LEFTMOST COLUMN
	BSR	PEONS_TB12REV3
	LDX	#$3D80	STARTING COLUMN
	LDA	#13
	STA	BCDR	WE WANT THIRTEEN NAMES IN A COLUMN
	LDA	#$15
	STA	BCDD	WERE STARTING AT NUMBER FIFTEEN IN THE TABLE
	BSR	PEONS_TB12REV3
	LDX	#$6780	STARTING COLUMN
	LDA	#13
	STA	BCDR	WE WANT THIRTEEN NAMES IN A COLUMN
	LDA	#$28
	STA	BCDD	WERE STARTING AT NUMBER TWENTY-EIGHT IN THE TABLE
	BSR	PEONS_TB12REV3

	LDX	#$1336	STARTING COLUMN
	LDY	#TODAYS
	LDB	#$11	COLOR 1
	LDU	#OUTBCD
	STU	BCDN
	LDU	#OUTCHR
	LDA	#3
	STA	BCDR	WE WANT THREE NAMES IN A COLUMN
	LDA	#$1
	STA	BCDD	WERE STARTING AT NUMBER ONE IN THE TABLE
	LDA	#$A
	STA	BCDN+2	WE WANT TEN LINES BETWEEN THE TOPS
	LDA	#21
	STA	BCDTMP+1 WE WANT IT 21 OVER FROM THE LEFTMOST COLUMN
	BSR	PEONS_TB12REV3
	LDX	#$5336	STARTING COLUMN
	LDA	#3
	STA	BCDR	WE WANT THREE NAMES IN A COLUMN
	LDA	#$4
	STA	BCDD	WERE STARTING AT NUMBER FOUR IN THE TABLE
	BSR	PEONS_TB12REV3


	PROCCR	VCYCLE,$40


	RTS

*	PEONS_TB12REV3 - A ROUTINE THAT WHEN CALLED WILL PUT OUT A COLUMN
*		WITH THE SPECIFIED FONT SIZE AND SPACING
*
*   REG B.=COLOR AND IS ASSUMED TO CONTAIN IT
*   REG X.=DESTINATION
*   REG Y.=CMOS LOCATION
*   REG U.=ROUTINE TO CALL (OUTCHR OR OUTC35)
*   BCDN  =ROUTINE TO CALL (OUTBCD OR OUTB35)
*   BCDN+2=NUMBER OF LINES FROM TOP TO TOP OF EACH LINE
*   BCDR  =NUMBER OF NAMES IN A COLUMN
*   BCDD  =STORAGE AREA FOR PLAYER POSITION IN TABLE
* BCDTMP+1=NUMBER TO SPACE OVER FROM START COLUMN
*
PEONS_TB12REV3:
	STX		SEED				SAVE THE STARTING DESTINATION
PEON_TB12REV3
	PSHS	B						SAVE REG B.
	LDB		#$03				THERE ARE 3 LETTERS
	STB		COLR				SAVE THIS FACT
	INCB							AND THERE ARE 4 BYTES OF SCORE
	STB		BCDTMP			SAVE THAT ALSO
	LDB		,S					GET REG B.
	CMPY	HSPP1				P1 ALL-TIME
	BEQ		23$
	CMPY	HSPP2				P1 DAILY
	BNE		20$
23$
	LDB		#RED
20$
	CMPY	HSPP12			P2 ALL-TIME
	BEQ		21$
	CMPY	HSPP22			P2 DAILY
	BNE		22$
21$
	LDB		#GREEN
22$
	LDA		BCDD				GET THE PLAYER POSITION IN THE TABLE
	BITA	#$F0				SHOULD WE BLANK THE LEADING ZERO
	BNE		2$					BRA=NOBLANKING NEEDED
	ORA		#$F0				BLANK IT
2$
	JSR		[BCDN]			USE THE SPECIFIED BCD OUTPUT ROUTINE
	LDA		#CBRKR			GET THE ')' CHARACTER
	JSR		,U					USE THE SPECIFIED CHARACTER OUTPUT ROUTINE
	LDA		#CSPC				PUT IN A SPACE
	JSR		,U					USE THE SPECIFIED CHARACTER OUTPUT ROUTINE
1$
	EXG		X,Y					RE: X=CMOS, Y=DEST
	JSR		RCMSA				GET THE CHARACTER
	EXG		X,Y					RE: X=DEST, Y=CMOS
	JSR		,U					GOTO THE SPECIFIED CHARACTER OUTPUT ROUTINE
	DEC		COLR				ONE LESS CHARACTER TO OUTPUT
	BNE		1$

* We just finished the #> and players initials
* Now we show their score

	STX		XTEMP				PUT REG X. AWAY SO AS TO MODIFY IT
	LDX		SEED				GET OUT INITIAL DESTINATION BACK
	EXG		D,X					SAVE THE D REG.
	LDB		XTEMP+1			GET THE LOWER BYTE OF THE CURRENT DESTINATION (Y coordinate)
	ADDA	BCDTMP+1		ADD THE SPACE BETWEEN LINES TO THE UPPER BYTE (New X Coordinate for the start of the Numbers to be printed)
	EXG		D,X					GET BACK D REG. AND WE'VE FORMED THE NEW DESTINATION

	CLR		BCDR+1			CLEAR THE TEST FOR BLANKING
3$
	EXG		X,Y					RE: X=CMOS, Y=DEST
	JSR		RCMSA				GET THE BCD NUMBER FROM CMOS
	EXG		X,Y					RE: X=DEST, Y=CMOS
	TST		BCDR+1			LETS SEE IF WE BLANKED THE DIGITS BEFORE US
	BNE		4$					BRA= THE DIGITS BEFORE US WERE NOT BLANKED SO..NO BLANK
	PSHS	A						SAVE A
	LDA		#$04				TO SEE IF THIS IS THE FIRST
	CMPA	BCDTMP			ARE WE ON THE FIRST SET OF DIGITS
	BNE		5$
	PULS	A
	BRA		6$
5$
	PULS	A
	BITA	#$F0				SHOULD WE BLANK THE UPPER DIGIT
	BNE		4$					BRA=NO WE SHOULDN'T
6$
	ORA		#$F0				BLANK THE DIGIT
	BITA	#$0F				SHOULD WE BLANK THE LOWER DIGIT
	BNE		4$					BRA= NO WE BETTER NOT
	ORA		#$0F				BLANK THE LOWER DIGIT
4$
	STA		BCDR+1			SAVE THE CURRENT CHARACTER
	COM		BCDR+1			COMPLEMENT BECAUSE. IF FULLY BLANKED THE DIGIT WHEN
*										COMPLEMENTED WILL BE EQUAL TO ZERO... A VERY EASY TEST
	JSR		[BCDN]			GOTO THE SPECIFIED BCD OUTPUT ROUTINE
	DEC		BCDTMP			ONE LESS BCD TO OUTPUT
	BNE		3$
	STX		XTEMP				PUT REG X. AWAY SO AS TO MODIFY IT
	LDX		SEED				GET OUT INITIAL DESTINATION BACK
	EXG		D,X					SAVE THE D REG.
	LDB		XTEMP+1			GET THE UPPER BYTE OF THE CURRENT DESTINATION
	ADDB	BCDN+2			ADD THE SPACE BETWEEN LINES TO THE UPPER BYTE
	EXG		D,X					GET BACK D REG. AND WE'VE FORMED THE NEW DESTINATION
	LDA		BCDD				GET THE CURRENT POSITION
	ADDA	#$01				INCREMENT BY 1
	DAA								MAKE IT INTO BCD
	STA		BCDD				AND SAVE IT
	DEC		BCDR				ONE LESS COLUMN TO DO
	PULS	B								GET B BACK
	LBNE	PEON_TB12REV3		AND LETS DO SOME MORE NAMES
	RTS										ALL DONE....NOW THAT DIDN'T HURT A BIT.......

*
*
* ACRED: ADDS A CREDITS TO TOTAL CREDITS; 99 MAX
*
ACRED_TB12REV3	PSHS	A,X	SAVE
	ADDA	CREDIT	GET PRESENT
	DAA		HUMANIZE
	BCC	ACRD0_TB12REV3	NO OVERFLOW
	LDA	#$99	YES, STAY AT 99
ACRD0_TB12REV3	STA	CREDIT	SAVE NEW COUNT
	LDX	#CREDST	BACKUP CREDITS
	JSR	WCMOSA_TB12REV3
	PULS	X,A	GO BACK CLEAN
	JMP	VCREDIT
*
* COIN SLOT ROUTINES
*
COINR_TB12REV3	PSHS	D,X	SAVE STATE
	LDB	#$03	3RD TOTALS(RIGHT COIN)
	BRA	COIN$	HANDLE BELOW
COINC_TB12REV3	PSHS	D,X	SAVE STATE
	LDB	#$02	2ND TOTALS(CENTER COIN)
	BRA	COIN$	HANDLE BELOW
COINL_TB12REV3	PSHS	D,X	SAVE STATE
	LDB	#$01	1ST TOTALS(LEFT COIN)
COIN$	JSR	AUD1_S_TB12REV3	BUMP COUNT BY 1
	ASLB	DOUBLE	FOR CMOS
	LDX	#SLOT1M-2 POINT TO START-2
	ABX		REMOVE OFFSET
	JSR	RCMOSB_TB12REV3	GET CORRECT SLOT X
	BSR	BCDHEX_TB12REV3	CONVERT TO BINARY
	LDA	BUNITS	GET PRESENT BONUS UNITS
	PSHS	B	AND ADD PRESENT COUNT TO IT
	ADDA	,S
	STA	BUNITS	UPDATE
	LDA	CUNITS
	ADDA	,S+	GET PRESENT
	STA	CUNITS	UPDATE
	LDX	#MINUNT	GET MINIMUM UNITS
	JSR	RCMOSB_TB12REV3	FROM CMOS
	BSR	BCDHEX_TB12REV3	WORK IN BINARY
	PSHS	B	FOR CBA
	CMPA	,S+	ENOUGH?
	BCC	COIN1_TB12REV3	YES, GIVE IT
	PULS	D,X,PC	NOT ENOUGH SO FAR, BYE
COIN1_TB12REV3	LDX	#CUNITC	HOW MANY?
	JSR	RCMOSB_TB12REV3	GET IT
	BSR	BCDHEX_TB12REV3	CONVERT TO HEX
	BSR	DIVAB_TB12REV3	SAVE REMAINDER IN B
	PSHS	A	SAVE COUNT TO BE AWARDED FOR A WHILE
	STB	CUNITS	SAVE REMAINDER
	LDX	#CUNITB	BONUS REQUIRES HOW MANY?
	JSR	RCMOSB_TB12REV3	IN B
	LDA	BUNITS	GET BONUS SO FAR
	BSR	BCDHEX_TB12REV3	CONVERT TO BINARY
	BSR	DIVAB_TB12REV3	DIVIDE
	TSTA		ANY YET?
	BEQ	COIN2_TB12REV3	NO
	CLR	CUNITS	YES, RESET ANY EXTRA
	CLR	BUNITS
COIN2_TB12REV3	ADDA	,S+	GET OTHER CREDITS EARNED
	DAA		HUMANIZE
	LDB	#$04	BUMP TOTPDC
	JSR	AUD_S_TB12REV3	BY THE COUNT PAID FOR
	BSR	ACRED_TB12REV3	ADD TO CREDITS COUNTER
	PULS	D,X,PC	CLEAN + GO
*
* DIVAB: A/B, REMAINDER IN B
*
DIVAB_TB12REV3	PSHS	B	SAVE
	TSTB		ANY?
	BNE	DIVAB0_TB12REV3	YES, HANDLE
	CLRA		NO, SET TO 0
	PULS	B,PC	CLEAN RETURN
DIVAB0_TB12REV3	EXG	A,B	FOR DAA
	LDA	#$99	START-1
DIVAB1_TB12REV3	ADDA	#$01	NEXT
	DAA		HUMANIZE
	SUBB	,S	TAKE AWAY N
	BCC	DIVAB1_TB12REV3	LOOP TILL OVERFLOW
	ADDB	,S+	ADD REMAINDER + FIX STACK
	RTS		BTE

*
*BCD-HEX CONVERT
*A=BCD-HEX
BCDHEX_TB12REV3	PSHS	A
	CLRA
BCH1_TB12REV3	CMPB	#$10
	BLO	BCH2_TB12REV3
	ADDA	#10
	SUBB	#$10
	BRA	BCH1_TB12REV3
BCH2_TB12REV3	PSHS	B
	ADDA	,S+
	TFR	A,B
	PULS	A,PC
*
*HEX-BCD CONVERT
*A=HEX-BCD
HEXBCD_TB12REV3	PSHS	B
	TFR	A,B
	CLRA
HBC1_TB12REV3	CMPB	#10
	BLO	HBC2_TB12REV3
	ADDA	#$10
	DAA
	SUBB	#10
	BRA	HBC1_TB12REV3
HBC2_TB12REV3	PSHS	B
	ADDA	,S+
	DAA
	PULS	B,PC
*

GODSET_TB12REV3	LDY	#$B000
	LDX	#GA1+2
	JSR	RCMOSB_TB12REV3
	BSR	BCDHEX_TB12REV3
	STB	_.NUMLET,Y
	LDB	#20
	LDA	#CSPC
	LEAX	_.CARAC,Y
5$	STA	,X+
	DECB
	BNE	5$
	LDX	#ENTINT_TB12REV3
	STX	_.WAKUP,Y
	LDX	#$2080
	STX	_.XSAVE,Y
	LDB	#CSPC
	STB	_.STCHR,Y
	CLR	_.CURLET,Y
	LDA	#CZ+1
	STA	_.NDCHR,Y
	LDA	#$3C
	STA	_.SIDE,Y
	LDA	#$77
	STA	_.COLOR,Y
1$	JSR	[_.WAKUP,Y]
	BCS	2$
	JSR	WAIT_TB12REV3
	BRA	1$
2$	LDB	#20
	LDX	#GODNAM
	LEAY	_.CARAC,Y
3$	LDA	,Y+
	JSR	WCMOSA_TB12REV3
	DECB
	BNE	3$
	RTS




*****************************************************************
*								*
*	ENTINT - GETS A CHARACTER FROM THE USERS CONTROLS	*
*		AND PUTS THE CHARACTER IN A BUFFER POINTED	*
*		TO BY REG Y. WHICH ALSO CONTAINS PARAMETERS	*
*		NECESSARY TO "ENTINT". SCREEN ECHO IS POINTED	*
*		TO BY REG X.					*
*								*
*	PARAMETERS NEEDED ARE:					*
*		@ _.NUMLET,Y - THE NUMBER OF LETTERS TO GET	*
*		@ _.CURLET,Y - AT START MUST BE CLEARED		*
*		@ _.STCHR,Y  - THE START OF THE CHARACTER SET	*
*		@ _.NDCHR,Y  - THE END OF THE CHARACTER SET	*
*		@ _.SIDE,Y   - THE CONTROL BYTE FOR THE WIDGET	*
*		@ _.COLOR,Y  - THE COLOR TO WRITE SCREEN ECHO IN	*
*								*
*	OUTPUTS ARE:						*
*		@ _.CARAC,Y  - THE START OF CHARACTER BUFFER	*
*								*
*	IF ENTINT IS NOT DONE IT WILL RETURN WITH THE CARRY	*
*	CLEAR OTHERWISE WHEN DONE THE CARRY WILL BE SET		*
*								*
*****************************************************************


ENTINT_TB12REV3:
	CLRA
	LDB	_.CURLET,Y	GET THE CURRENT LETTER TO INPUT
	BEQ	21$
20$	PSHS	B,X
	DECB
	ADDB	#_.CARAC
	LDB	B,Y
	ASLB
	LDX	FONT5
	ADDA	[B,X]
	PULS	B,X
	DECB
	BNE	20$
21$	LDX	_.XSAVE,Y
	LEAX	D,X	AND USE A 16 BIT OFFSET
	STX	_.CURPOS,Y
	LDB	#_.CARAC	GET THE CHARACTER STORAGE AREA
	ADDB	_.CURLET,Y	GET THE CURRENT LETTER
	LDA	B,Y
	JSR	WRCUR_TB12REV3	WRITE THE CURSOR
	LDB	_.SIDE,Y	GET THE CONTROL BYTE TO SEND TO THE WIDGET
* IF B = $3C then player 1 input, $34 is player 2 input requested
;	STB	PIA3+1	AND GIVE IT TO IT
  CMPB  #$3C
  BNE   >
* Get P1 buttons in B
      LDB   WPIAA_P1
      BRA   ENTINT_TB12REV3_21
!
* Get P2 buttons in B
      LDB   WPIAA_P2
ENTINT_TB12REV3_21:
	ANDB	#$07	WE ONLY WANT 'MOVE LEFT','MOVE RIGHT','FALP'
	BNE	40$	BRA= SWITCHES HAVE BEEN PRESSED
	LDB	_.DEBONC,Y
	LBEQ	ENTRET_TB12REV3
	DEC	_.DEBONC,Y
	LBRA	ENTRET_TB12REV3
40$	LDX	#ENTI2_TB12REV3	DEBOUNCE THEM A BIT
	STX	_.WAKUP,Y	GET THE WAKE-UP ADDRESS
	ANDCC	#$FE	CLEAR CARRY
	RTS		GO AND SLEEP

ENTI2_TB12REV3:
	PSHS	B
	LDB	_.SIDE,Y
* IF B = $3C then player 1 input, $34 is player 2 input requested
;	STB	PIA3+1	AND GIVE IT TO IT
  CMPB  #$3C
  BNE   >
* Get P1 buttons in B
      PULS   B
      CMPB   WPIAA_P1
  BNE	ENTRET_TB12REV3	BRA= NO SO IT'S PROBBALY NOISE RETURN TO THE CALLER
      BRA    ENTI2_TB12REV3_Continue
!
* Get P2 buttons in B
      PULS   B
      CMPB   WPIAA_P2
;	PULS	B
;	CMPB	PIA2	IS THE SAME SWITCH STILL PRESSED
	BNE	ENTRET_TB12REV3	BRA= NO SO IT'S PROBBALY NOISE RETURN TO THE CALLER
ENTI2_TB12REV3_Continue:
	LSRB		SHIFT IT BECAUSE IF IT'S BIT 0 THEN IT WILL NOW BE 0
	BNE	2$	BRA= IT'S NOT THIS SWITCH
	LDB	_.DEBONC,Y
	BEQ	1$
	DEC	_.DEBONC,Y
1$	BSR	ERSHSC_TB12REV3	GO AND ERASE THE CHARACTER
	CMPA	_.STCHR,Y	IS THIS THE START CHARACTER
	BNE	3$	BRA= NO IT'S NOT THE BEGINNING
	LDA	_.NDCHR,Y	GET THE LAST CHARACTER "WRAP-AROUND"
	BRA	ENTRET_TB12REV3	WE WANT TO BRANCH OVER THE DECREMENT
3$	DECA		MOVE TO THE NEXT CHARACTER
	BRA	ENTRET_TB12REV3	GO WRITE AND THE CHARCATER
2$	LSRB		SHIFT IT BECAUSE IF IT'S BIT 1 THEN IT WILL NOW BE 0
	BNE	5$	BRA= IT'S NOT THIS SWITCH
	LDB	_.DEBONC,Y
	BEQ	10$
	DEC	_.DEBONC,Y
10$	BSR	ERSHSC_TB12REV3	GO AND ERASE THE CHARACTER
	CMPA	_.NDCHR,Y	IS THIS THE END CHARACTER
	BNE	6$	BRA= NO IT'S NOT THE START
	LDA	_.STCHR,Y	GET THE FIRST CHARACTER "WRAP-AROUND"
	BRA	ENTRET_TB12REV3	WE WANT TO BRANCH OVER THE INCREMENT
6$	INCA		MOVE TO THE NEXT CHARACTER
	BRA	ENTRET_TB12REV3	AND NOW WRITE THE CHARACTER
5$	LDB	_.DEBONC,Y
	BEQ	30$
	DEC	_.DEBONC,Y
	BRA	ENTRET_TB12REV3
30$	LDB	#$2
	STB	_.DEBONC,Y
	CMPA	#CBARRW	IS THE CHARACTER HE ENTERED THE BACK-ARROW
	BNE	8$	BRA= NO SO IT'S A NORMAL CHARACTER
	TST	_.CURLET,Y	IS THIS THE FIRST CHARACTER
	BEQ	ENTRET_TB12REV3	BRA= NICE TRY BUT YOU CAN'T FOOL ME
	BSR	ERSHSC_TB12REV3	OK WE'LL LET YOU GO BACK ONE CHARACTER
	INC	_.NUMLET,Y	ONE MORE CHARACTER TO INPUT
	DEC	_.CURLET,Y	BACK THE CURRENT LETTER POINTER 1
	BSR	ERCUR_TB12REV3	ERASE THE CURSOR
	BRA	ENT10$	NOW THAT YOUR BACK ONE LET'S RETURN TO THE CALLER
8$	INC	_.CURLET,Y	MOVE THE POINTER TO THE NEXT CHARACTER
	BSR	ERCUR_TB12REV3	ERASE THE CURSOR
	DEC	_.NUMLET,Y	ONE LESS CHARACTER TO INPUT
	BNE	ENTRET_TB12REV3	IT'S NOT ZERO SO RETURN TO THE CALLER
	ORCC	#$01	IT'S ZERO SO RETURN WITH CARRY SET TO INDICATE DONE
ENT10$	LDX	#ENTINT_TB12REV3
	STX	_.WAKUP,Y
	RTS		RETURN TO THE CALLER
ENTRET_TB12REV3	BSR	OUTHSC_TB12REV3	WRITE THE CHARACTER
	ANDCC	#$FE	CLEAR CARRY
	BRA	ENT10$

OUTHSC_TB12REV3	LDB	#_.CARAC	GET THE CHARACTER STORAGE AREA
	ADDB	_.CURLET,Y	POINT AT THE CURRENT LETTER
	STA	B,Y	SAVE THE LETTER
	LDB	_.COLOR,Y	GET THE COLOR TO WRITE IN
OUT1_TB12REV3	LDX	_.CURPOS,Y	GET THE SCREEN CURSOR POSITION
	JSR	OUTCHR	WRITE THE CHARACTER
	RTS		RETURN

ERSHSC_TB12REV3	LDB	#$00	COLOR ZERO "BACKGROUND"
	BRA	OUT1_TB12REV3	REUSE SOME CODE

WAIT_TB12REV3	PSHS	D	SAVE THE STATE OF THE UNION
	LDA	#$04	WE WANT ABOUT 16 X 16MS.
	JSR	NAPV	AND LET'S DOSE OFF
	PULS	D,PC	RETURN

WRCUR_TB12REV3	PSHS	D
	LDB	_.COLOR,Y
WCUR_TB12REV3	LDA	#CCURS
	LDX	_.CURPOS,Y
	JSR	OUTCHR
	PULS	D,PC

ERCUR_TB12REV3	PSHS	D
	LDB	#$00
	BRA	WCUR_TB12REV3



OPOUT_TB12REV3	LDB	#$66
	STB	COLR
	LDB	#$88	GET THE Y COORDINATE
	BSR	OPO1_TB12REV3
	LDB	#$9A	GET THE Y COORDINATE
	BRA	OPO2_TB12REV3

OPO1_TB12REV3	PSHS	D,X,Y
	LDX	#OPL1	GET THE X COORDINATE FROM CMOS
	JSR	RCMSA	READ IT
	TFR	D,Y	PUT THE STARTING COORDINATE IN REG Y.
	LDX	#OPMESS	GET THE START OF THE MESSAGE
	LDB	COLR	GET THE COLOR
1$	JSR	RCMSA	READ IN A CHARACTER
	EXG	X,Y	EXCHANGE CMOS WITH THE SCREEN
	JSR	OUTCHR	WRITE THE CHARACTER TO THE SCREEN
	EXG	X,Y	EXCHANGE THE SCREEN WITH CMOS
	CMPX	#OPMESS+50	ARE AT THE END OF THE FIRST LINE
	BNE	1$	BRA= NO SO GO AND GET ONE MORE
	PULS	D,X,Y,PC	WERE DONE SO LET'S RETURN

OPO2_TB12REV3	PSHS	D,X,Y
	LDX	#OPL2	GET THE X COORDINATE FROM CMOS
	JSR	RCMSA	READ IT
	TFR	D,Y	PUT THE STARTING COORDINATE IN REG Y.
	LDX	#OPMESS+50	GET THE START OF THE MESSAGE
	LDB	COLR	GET THE COLOR
2$	JSR	RCMSA	READ IN A CHARACTER
	EXG	X,Y	EXCHANGE CMOS WITH THE SCREEN
	JSR	OUTCHR	WRITE THE CHARACTER TO THE SCREEN
	EXG	X,Y	EXCHANGE THE SCREEN WITH CMOS
	CMPX	#OPMESS+100	ARE AT THE END OF THE SECOND LINE
	BNE	2$	BRA= NO SO GO AND GET ONE MORE
	PULS	D,X,Y,PC	WERE DONE SO LET'S RETURN

OPST_TB12REV3	LDY	#$B000
	STX	_.XSAVE,Y
	LDD	#ENTINT_TB12REV3
	STD	_.WAKUP,Y
	LDB	#25
	STB	_.NUMLET,Y
	LDB	#20
	LDA	#CSPC
	LEAX	_.CARAC,Y
5$	STA	,X+
	DECB
	BNE	5$
	STB	_.CURLET,Y
	STB	_.STCHR,Y
	CLR	_.CARAC,Y
	LDA	#CCOLN
	STA	_.NDCHR,Y
	LDA	#$3C
	STA	_.SIDE,Y
	LDA	#$77
	STA	_.COLOR,Y
1$	JSR	[_.WAKUP,Y]
	BCS	>
	JSR	WAIT_TB12REV3
	BRA	1$

!	RTS

OPENT_TB12REV3	LDX	#$2560
	BSR	OPST_TB12REV3
	LDB	#25
	LDX	#OPMESS
	LEAY	_.CARAC,Y
3$	LDA	,Y+
	JSR	WCMOSA_TB12REV3
	DECB
	BNE	3$
	LDA	#$25
	LDX	#OPL1
	JSR	WCMOSA_TB12REV3
	LDB	#$88
	BSR	TXCEN_TB12REV3
	LDA	#CCNARW	GET THE CENTERING ARROW
	LDB	#$22	GET THE COLOR
	LDX	#$486A	POSITION FOR CENTER
	JSR	OUTCHR	WRITE IT OUT
	LDU	#OPE2_TB12REV3	GET THE EXIT ADDRESS
1$	LDX	#OPL1	GET THE X COORDINATE
	JSR	RCMOSB_TB12REV3	READ IT
	JSR	LTRT_TB12REV3	LET HIM ADJUST IT
	STB	SW0ST
	CLR	COLR	MAKE THE COLOR 0
	LDB	#$60	GET THE Y COORDINATE
	JSR	OPO1_TB12REV3	ERASE IT
	LDA	SW0ST
	LDX	#OPL1
	JSR	WCMOSA_TB12REV3	SAVE THE NEW COORDINATE
	LDA	#$22
	STA	COLR
	JSR	OPO1_TB12REV3	WRITE IT
	BRA	1$


TXCEN_TB12REV3	LDA	#MSPCEN	WRITE THE CENTERING MESSAGE OUT
	LDX	#$2590
	JSR	OUTPHR
	LDA	#MSAENT	WRITE THE CENTERING MESSAGE OUT
	LDX	#$25A0
	JMP	OUTPHR


OPE2_TB12REV3	LDA	#CCNARW	GET THE CENTERING ARROW
	LDB	#00	GET THE COLOR
	LDX	#$486A	POSITION FOR CENTER
	JSR	OUTCHR	WRITE IT OUT
	BSR	TXCEN_TB12REV3
	LDX	#$2570
	JSR	OPST_TB12REV3
	LDB	#25
	LDX	#OPMESS+50
	LEAY	_.CARAC,Y
3$	LDA	,Y+
	JSR	WCMOSA_TB12REV3
	DECB
	BNE	3$
	LDA	#$25
	LDX	#OPL2
	JSR	WCMOSA_TB12REV3
	LDB	#$88
	BSR	TXCEN_TB12REV3
	LDA	#CCNARW	GET THE CENTERING ARROW
	LDB	#$22	GET THE COLOR
	LDX	#$487A	POSITION FOR CENTER
	JSR	OUTCHR	WRITE IT OUT
	LDU	#OPEND_TB12REV3	GET THE EXIT ADDRESS
2$	LDX	#OPL2	GET THE X COORDINATE
	JSR	RCMOSB_TB12REV3	READ IT
	BSR	LTRT_TB12REV3	LET HIM ADJUST IT
	STB	SW0ST
	CLR	COLR	MAKE THE COLOR 0
	LDB	#$70	GET THE Y COORDINATE
	JSR	OPO2_TB12REV3	ERASE IT
	LDA	SW0ST
	LDX	#OPL2
	JSR	WCMOSA_TB12REV3	SAVE THE NEW COORDINATE
	LDA	#$22
	STA	COLR
	JSR	OPO2_TB12REV3	WRITE IT
	BRA	2$
OPEND_TB12REV3	RTS


LTRT_TB12REV3	LDA	PIA0
	BITA	#$02
	BEQ	3$
	LEAS	2,S	GET RID OF THE RETURN ADDRESS
	JMP	,U
3$	LDA	PIA2	READ THE SWITCH
	ANDA	#$03	ONLY NEED LEFT OR RIGHT
	JSR	WAIT_TB12REV3
	CMPA	PIA2	IS THE SAME SWITCH STILL PRESSED
	BNE	LTRT_TB12REV3
	TSTA
	BEQ	LTRT_TB12REV3
	LSRA		SHIFT IT
	BNE	2$	BRA= IT'S NOT THIS SWITCH
	CMPB	#$18	IS IT AT THE RIGHT-MOST EDGE
	BEQ	LTRT_TB12REV3	BRA= YES SO DO NOTHING
	DECB		DECREMENT THE X COORDINATE
	RTS
2$	LSRA		SHIFT THE SWITCH
	BNE	LTRT_TB12REV3	BRA= THE SWITCH IS INVALID
	CMPB	#$40	IS IT AT THE LEFT-MOST EDGE
	BEQ	LTRT_TB12REV3	BRA= YES SO DO NOTHING
	INCB		INCREMENT THE X COORDINATE
	RTS

OVERFLO_TB12REV3	LDA	,X
	ANDA	#$F0
	BEQ	1$
	LDD	#$9999
	STD	,X++
	STD	,X++
1$	RTS


ENDGAM_TB12REV3	CLR	ANYONE	CLEAR THE DID ANYONE MAKE THE TABLE FLAG
	LDA	N2SHIP+1	ARE HIGH SCORES ALLOWED?
	LSRA
	BCS	90$	BRA= YES
	JMP	VATTRT
90$	LDX	#SPLY1
	BSR	OVERFLO_TB12REV3
	LDX	#SPLY2
	BSR	OVERFLO_TB12REV3
	TST	NPLYRS
	BEQ	1$
	LDX	#SPLY1	PLAYER 1'S SCORE
	LDY	#SPLY2	PLAYER 2'S SCORE
	JSR	CMPSCR_TB12REV3
	BCS	1$	BRA= PLAYER 1 IS GREATER
	EXG	Y,X
	JSR	GODCHK_TB12REV3
	BCC	2$
	INC	ANYONE	TELL THEM THAT SOMEONE HAS GONE TO THE TABLE
	PROCCR	GODESB_TB12REV3,$42   START GOD PROCESS BUT DON'T RELEASE CONTROL YET.
	LDA	#$10
	STA	PNAP,Y
	LDA	#$1
	STA	_.PLYNUM,Y	INDICATE THAT IT IS PLAYER 2
	LDX	#SPLY2
	STX	_.SCRSV,Y
	LDX	#$70A0	WHERE HE IS TO ENTER
	STX	_.AREA,Y
	BRA	7$
2$	LDX	#SPLY2
	STX	_.SCRSV,U
	LDX	#$70A0	WHERE HE IS TO ENTER
	STX	_.AREA,U
	LDA	#$1
	LDB	#$42
	BSR	EGSUB1_TB12REV3	CHECK IF HE'S A PEON
7$	LDX	#SPLY1
	STX	_.SCRSV,U
	LDX	#$20A0	WHERE HE IS TO ENTER
	STX	_.AREA,U
	CLRA
	LDB	#$41
	BSR	EGSUB1_TB12REV3	CHECK IF HE'S A PEON
	BRA	6$
1$	LDX	#SPLY1
	JSR	GODCHK_TB12REV3
	BCC	4$
	INC	ANYONE	TELL THEM THAT SOMEONE HAS GONE TO THE TABLE
	PROCCR	GODESB_TB12REV3,$42   START GOD PROCESS BUT DON'T RELEASE CONTROL YET.
	LDA	#$10
	STA	PNAP,Y
	CLRA
	STA	_.PLYNUM,Y	INDICATE THAT IT IS PLAYER 1
	LDX	#SPLY1
	STX	_.SCRSV,Y
	LDX	#$20A0	WHERE HE IS TO ENTER
	STX	_.AREA,Y
	BRA	5$
4$	LDX	#SPLY1
	STX	_.SCRSV,U
	LDX	#$20A0	WHERE HE IS TO ENTER
	STX	_.AREA,U
	CLRA
	LDB	#$42
	BSR	EGSUB1_TB12REV3	CHECK IF HE'S A PEON
5$	TST	NPLYRS
	BEQ	6$
	LDX	#SPLY2
	STX	_.SCRSV,U
	LDX	#$70A0	WHERE HE IS TO ENTER
	STX	_.AREA,U
	LDA	#$1
	LDB	#$41
	BSR	EGSUB1_TB12REV3	CHECK IF HE'S A PEON
6$	JMP	AMODE_TB12REV3	JUMP TO GAME OVER (AMODE) PROCESSING


EGSUB1_TB12REV3	JSR	TODCHK_TB12REV3	CHECK TODAYS HIGH
	BCC	1$
	INC	ANYONE	TELL THEM THAT SOMEONE HAS GONE TO THE TABLE
	PSHS	A
	LDX	#GETHIM_TB12REV3	START ENTER PROCESS BUT DON'T RELEASE CONTROL YET.
	TFR	B,A
	CLRB
	JSR	VCRPROC
	LDA	#$10
	STA	PNAP,Y
	PULS	A
	STA	_.PLYNUM,Y
	LDX	_.SCRSV,U
	STX	_.SCRSV,Y
	LDX	_.AREA,U
	STX	_.AREA,Y
	RTS
*
1$	JSR	ALLCHK_TB12REV3	CHECK ALL TIME
	BCC	2$
	INC	ANYONE	TELL THEM THAT SOMEONE HAS GONE TO THE TABLE
	PSHS	A
	LDX	#GETHIM_TB12REV3	START ENTER PROCESS BUT DON'T RELEASE CONTROL YET.
	TFR	B,A
	CLRB
	JSR	VCRPROC
	LDA	#$08
	STA	PNAP,Y
	PULS	A
	STA	_.PLYNUM,Y
	LDX	_.SCRSV,U
	STX	_.SCRSV,Y
	LDX	_.AREA,U
	STX	_.AREA,Y
2$	RTS

*
GETHIM_TB12REV3	CLR	_.GODFLG,U
GETHM2_TB12REV3	LDB	_.PLYNUM,U	GET PLAYER NUMBER
	BNE	2$
	LDX	#$0AB0
	LDB	#$3C
	PSHS	B
	LDB	#RED
	BRA	3$
2$	LDX	#$58B0
	LDB	#$34
	PSHS	B
	LDB	#GREEN
3$	LDA	#MSPEON
	JSR	OUTPHR	PUT THE MESSAGE OUT
	TFR	B,A	PUT THE COLOR IN REG A.
	LEAY	PPOSX,U	GET THE BUFFER IN THE PROCESS AREA
	LDB	#2		***PFUT, ASSUME SWITCH ON TO START
	STB	_.DEBONC,Y	***PFUT, ASSUME SWITCH ON TO START
	PULS	B
	STB	_.SIDE,Y
********	CLR	_.DEBONC,Y	***PFUT, ASSUME SWITCH ON TO START
	LDX	#1$
	STX	_.RETURN,Y	SAVE IT
	LDX	_.AREA,U	PLACE TO DO IT
	JSR	SETPEON_TB12REV3
1$	LDA	PID,U
	CMPA	#$42
	BEQ	31$
	STD	_.SAVEA,U
	STX	_.SAVEX,U
	STY	_.SAVEY,U
30$	PCNAP	1
	LDX	PLINK,U
	LDA	PID,X
	CMPA	#$42
	BEQ	30$
	LDD	_.SAVEA,U
	LDX	_.SAVEX,U
	LDY	_.SAVEY,U
31$	JSR	TODCHK_TB12REV3	SEE WHERE TO PUT IT ETC.
	BCC	GETHM3_TB12REV3	NOT THIS TIME JACK
	TST	_.PLYNUM,U
	BNE	4$
	STY	HSPP2
	BRA	5$
4$	STY	HSPP22
5$	LDX	#TODEND-SCRSIZ END OF TODAYS
	JSR	SCTRNS_TB12REV3
GETHM3_TB12REV3	JSR	ALLCHK_TB12REV3	CHECK FOR ALL TIME
	BCC	GET789_TB12REV3	NOPE
	TST	_.GODFLG,U	WAS THIS GOD??
	BEQ	GETHM4_TB12REV3	NOPE
GODSPC_TB12REV3	LEAX	PPOSX,U
	LEAX	_.CARAC,X
	LDY	#GODINT	JUST STORE THE INITIALS ON THIS SIDE
	LDB	#3
	JSR	CMOSMV_TB12REV3	DONE.
	JSR	MKGODC_TB12REV3	FORM GOD'S NEW CHECK BYTE
	LDA	#5	INTERESTED IF THERE ARE 5 OTHERS!
	BSR	SETB_SS_TB12REV3	WELL????
GET789_TB12REV3	BCC	GETHM5_TB12REV3	NOPE.
	TFR	X,Y	MOVE LOWEST POINTED TO INTO Y
	JSR	RMENTR_TB12REV3	REMOVE IT.
	BRA	GTTHM8_TB12REV3	AND TELL HIM IT WAS REMOVED
GETHM4_TB12REV3	LEAX	$329A,Y
	BNE	80$
	LDX	#TODAYS-SCRSIZ
	JSR	BUBDN_TB12REV3
	LEAY	6,Y
	LDX	_.SCRSV,U
	LDB	#4
	JSR	CMOSMV_TB12REV3
	LDY	#GODNAM
	TST	_.PLYNUM,U
	BNE	84$
	STY	HSPP1
	BRA	85$
84$	STY	HSPP12
85$	LEAX	PPOSX,U
	LEAX	_.CARAC,X
	LDB	#20
	JSR	CMOSMV_TB12REV3
	BRA	GODSPC_TB12REV3
80$	JSR	SETBOT_TB12REV3	FIND PLACE TO BUBBLE DOWN TO (5 MAX RULE)
	PSHS	CC	SAVE ANSWER
	PSHS	X
	CMPY	,S++
	BHI	GETHM8_TB12REV3	NOT BETTER THAN HIS TOP 5
	BSR	SCTRNS_TB12REV3	DONE.
	TST	_.PLYNUM,U
	BNE	>
	STY	HSPP1
	BRA	GETHM8_TB12REV3
!	STY	HSPP12
GETHM8_TB12REV3	PULS	CC	GET CARRY BACK
	BCC	GETHM5_TB12REV3
GTTHM8_TB12REV3	LDX	#HSPP1	START AT PLAYER 1 SAVE
	LEAY	PPOSX,U
	LDX	_.AREA,U
	LEAX	-$150A,X
	LDA	#ONLY5M
	LDB	_.COLOR,Y
	JSR	OUTPHR
	PCNAP	$60
GETHM5_TB12REV3	JMP	VSUCIDE		end the process

SETB_SS_TB12REV3	PSHS	Y,D
	BRA	SETBT0_TB12REV3

SETBOT_TB12REV3	PSHS	Y,D
	LDX	#GODINT	SEE IF MATCH OF GOD INITIALS
	BSR	SETSUB_TB12REV3
	LDA	#4	ASSUME MATCH
	BCS	SETBT0_TB12REV3	YEP...ONLY 4 MORE NEEDED
	INCA
SETBT0_TB12REV3	STA	XTEMP
	LDX	#CMSCOR	POINT AT FIRST ENTRY
SETBT1_TB12REV3	BSR	SETSUB_TB12REV3	SEE IF THIS ONE MATCHES
	BCC	SETBT3_TB12REV3	NOPE...
	DEC	XTEMP	FOUND
	BEQ	SETBT5_TB12REV3	AND TIME TO EXIT
SETBT3_TB12REV3	LEAX	SCRSIZ,X PUSH TO NEXT
	CMPX	#TODAYS
	BLO	SETBT1_TB12REV3	NOT DONE...DO ANOTHER
	LDX	#TODAYS-SCRSIZ BUBBLE FROM BOTTOM
	ANDCC	#$FE	(CLC)
	PULS	Y,D,PC
SETBT5_TB12REV3	ORCC	#$01	SEC
	PULS	Y,D,PC	AND RETURN IT

SETSUB_TB12REV3	PSHS	X
	LEAY	PPOSX,U	ALT TABLE
	LEAY	_.CARAC,Y
	LDB	#3
SETSB2_TB12REV3	JSR	RCMOSA_TB12REV3
	CMPA	,Y+	COMPARE TO ALT LETTER
	BNE	SETSB1_TB12REV3	NO GOOD...MOVE TO NEXT
	DECB		ONE LESS TO DO
	BNE	SETSB2_TB12REV3
	ORCC	#$01	SEC
	PULS	X,PC
SETSB1_TB12REV3	ANDCC	#$FE	(CLC) NO MATCH
	PULS	X,PC

SCTRNS_TB12REV3	PSHS	Y
	JSR	BUBDN_TB12REV3	BUBBLE EM TO MAKE ROOM
	LEAX	PPOSX,U
	LEAX	_.CARAC,X
	LDB	#3
	JSR	CMOSMV_TB12REV3	TRANSFER THE BLOCK
	LDX	_.SCRSV,U
	LDB	#4
	JSR	CMOSMV_TB12REV3	TRANSFER THE SCORE
	PULS	Y
	JMP	MKSCCK_TB12REV3	FORM THE CHECK BYTE

BUBDN_TB12REV3	PSHS	X,Y
	TFR	X,Y	BOTTOM IS DESTINATION
BUBDN1_TB12REV3	CMPY	2,S	IS OUR DESTINATION WHAT WE WANT TO FILL??
	BEQ	BUBDUN_TB12REV3	THEN WE'RE DONE
	LEAX	-SCRSIZ,Y X IS SOURCE
	LDA	#SCRSIZ	THAT MANY BYTES
	JSR	BLKMOV_TB12REV3	MOVE THE BLOCK
	LEAY	-SCRSIZ,Y
	BRA	BUBDN1_TB12REV3
BUBDUN_TB12REV3	PULS	X,Y,PC	BYE



GODESB_TB12REV3	INC	_.GODFLG,U	FOR LATER USE
	LEAY	PPOSX,U	GET THE BUFFER IN THE PROCESS AREA
	LDA	_.PLYNUM,U
	BNE	1$
	LDA	#$3C	PLAYER 1 PIA SIDE
	STA	_.SIDE,Y
	LDA	#RED
	BRA	2$
1$	LDA	#$34	PLAYER 2 PIA SIDE
	STA	_.SIDE,Y
	LDA	#GREEN
2$	TFR	A,B
	LDA	#MSGOD
	LDX	#$295B	WHERE TO PUT THE ENTER MESSAGE
	JSR	OUTPHR	WRITE IT
	TFR	B,A
	LDX	#3$
	STX	_.RETURN,Y
	JMP	SETGOD_TB12REV3
3$	LEAX	_.CARAC,Y
	LDY	#GODNAM
	LDB	#20
	JSR	CMOSMV_TB12REV3
	LDY	#GODINT	BUBBLE TO VACATE THIS POSITION
	LDX	#TODAYS-SCRSIZ FROM BOTTOM
	JSR	BUBDN_TB12REV3	BUBBLE EM DOWN
	LDY	#CMSCOR	POINT AT #2
	JSR	MKSCCK_TB12REV3	AND FORM HIS CHECK BYTE
	LDX	_.SCRSV,U	POINT AT SCORE
	LDY	#GODSCR
	LDB	#4
	JSR	CMOSMV_TB12REV3
	LDX	#NULSCR_TB12REV3	BLANK INITIALS
	LDY	#GODINT
	LDB	#3
	JSR	CMOSMV_TB12REV3
	JSR	MKGODC_TB12REV3
	LDX	#GODNAM
	LDA	_.PLYNUM,U
	BNE	80$
	STX	HSPP1
	BRA	81$
80$	STX	HSPP12
81$	JMP	GETHM2_TB12REV3	JUMP INTO THE ROUTINE TO DO THAT.

* GODCHK - THIS ONE ASKS "WELL, DID HE BEAT GOD?"
*		AND RETURNS WITH CARRY SET IF HE DID.

GODCHK_TB12REV3	LDY	#GODSCR	POINT AT THE SCORE PART
	BRA	CMPSCR_TB12REV3	COMPARE SCORES
*	RETURN THE CARRY BIT

* TODCHK - WILL CHECK TO SEE IF THE PLAYER SPECIFIED
*		IN _.SCRSV,U IS AMONGST THE DAILY PEONS (BUZZARDS)

TODCHK_TB12REV3	PSHS	A,X	RETURN ENTRY POINTER IF OK
	LDY	#TODAYS+6
	LDX	_.SCRSV,U
TODCK1_TB12REV3	BSR	CMPSCR_TB12REV3	COMPARE
	BCS	TODBYE_TB12REV3
	LEAY	SCRSIZ,Y PUSH TO NEXT
	CMPY	#TODEND	BEYOND
	BLO	TODCK1_TB12REV3
	ANDCC	#$FE	(CLC)
	PULS	A,X,PC
TODBYE_TB12REV3	LEAY	-6,Y	PUSH BACK TO BEGINNING OF ENTRY TO REPLACE
	PULS	A,X,PC	AND RETURN WITH CARRY SET

* ALLCHK - WILL CHECK TO SEE IF THE PLAYER SPECIFIED
*		IN _.SCRSV,U IS AMONGST THE ALLTIME PEONS (JOUST CHAMPIONS)
*			AND EVEN CHECK IF HE'S BEAT GOD

ALLCHK_TB12REV3	PSHS	A,X
	LDY	#GODSCR	POINT AT SCORE PART OF GOD ENTRY
	LDX	_.SCRSV,U
ALCK1_TB12REV3	BSR	CMPSCR_TB12REV3
	BCS	TODBYE_TB12REV3
	LEAY	SCRSIZ,Y
	CMPY	#TODAYS-SCRSIZ BEYOND VISIBLES??????
	BLO	ALCK1_TB12REV3	NOPE..CONTINUE
	ANDCC	#$FE	CLC
	PULS	A,X,PC	NOT GOOD ENOUGH

* CMPSCR - X=MEMORY LOCATION, Y=CMOS OR MEMORY LOCATION, D IS SAVED
*	THIS WILL ASK THE DEVINE QUESTION:
*		IS MEMORY-X GREATER THAN MEMORY-Y ?
*			IF TRUE THIS WILL SET THE CARRY BIT.

CMPSCR_TB12REV3	PSHS	Y,X,D
	EXG	X,Y	MAKE X POINT AT CMOS
	LDB	#4	4 BYTES THROUGH
CMPSC1_TB12REV3	BSR	NEWCHK_TB12REV3	THIS IS SO WE CAN COMPARE MEMORY TO MEMORY
	CMPB	#4	FIRST TIME THROUGH??
	BNE	CMPSC2_TB12REV3
	ANDA	#$0F	THEN TAKE AWAY CHECK BYTE
CMPSC2_TB12REV3	CMPA	,Y+	COMPARE TO PLAYERS SCORE
	BHI	CMPSC4_TB12REV3	CMOS SCORE HIGHER
	BLO	CMPSC5_TB12REV3	PLAYER'S SCORE HIGHER
	DECB		SAME...ARE WE DONE??
	BNE	CMPSC1_TB12REV3	NOPE...COMPARE ANOTHER BYTE
CMPSC4_TB12REV3	ANDCC	#$FE	CLC
	PULS	X,Y,D,PC
CMPSC5_TB12REV3	ORCC	#$01	SEC PLAYER IS HIGHER
	PULS	X,Y,D,PC

NEWCHK_TB12REV3	CMPX	#$C000	IS THIS A CMOS MEMORY LOCATION
	BLO	1$	BRA= NO IT'S IN SCRATCH
	JSR	RCMOSA_TB12REV3	READ A BYTE
	RTS
1$	LDA	,X+	READ MEMORY AND MOVE AHEAD ALSO
	RTS

*************************************************
* VALUES NEEDED UPON ENTRY:			*
*						*
*	FOR SETGOD:				*
*		REG A.=COLOR TO WRITE IN	*
*						*
*	FOR SETPEON:				*
*		REG A.=COLOR TO WRITE IN	*
*		REG B.=NUMBER OF LETTERS TO GET	*
*		REG X.=WHERE TO ECHO ON SCREEN	*
*		REG Y.=MEMORY BUFFER LOCATION	*
*************************************************
*	LDY	#$XXXX	MEMORY BUFFER
*	STA	_.SIDE,Y	THESE MUST BE DEFINED BEFORE CALLING

SETGOD_TB12REV3	LDX	#GA1+2	GET THE CMOS VALUE FOR THE NUMBER OF LETTERS TO ENTER
	JSR	RCMOSB_TB12REV3	READ IT
	JSR	BCDHEX_TB12REV3	CONVERT IT TO HEX
	LDX	#$2643	APPROXIMATE AREA FOR GOD'S SCREEN ECHO
	BRA	SETP1_TB12REV3
SETPEON_TB12REV3	LDB	#$03	PEONS ONLY GET THREE LETTERS
SETP1_TB12REV3	STA	_.COLOR,Y	SAVE THE COLOR TO WRITE IN
	STB	_.NUMLET,Y	SAVE THE NUMBER OF LETTERS TO GET
	STX	_.XSAVE,Y	SAVE THE START OF THE SCREEN ECHO
	LDB	#20	FILL A GENERAL SIZE BUFFER WITH SPACES
	LDA	#CSPC		|
	LEAX	_.CARAC,Y	|
5$	STA	,X+		|
	DECB			|
	BNE	5$	--------|
	LDX	#ENTINT_TB12REV3	GET THE START OF THE INITIAL ENTERING ROUTINE
	STX	_.WAKUP,Y	SAVE IT. SO AS TO DO A JSR OFF OF IT
	LDB	#CSPC	GET A SPACE AGAIN
	CLR	_.CURLET,Y
	STB	_.STCHR,Y	MAKE IT FIRST LETTER OF THE GROUP HE CAN ENTER
*	CLR	_.CARAC,Y
	LDA	#CZ+1	GET THE BACK-ARROW
	STA	_.NDCHR,Y	MAKE IT LAST LETTER OF THE GROUP HE CAN ENTER
1$	JSR	[_.WAKUP,Y]
	BCS	2$
	STY	_.SAVEY,U
	STD	_.SAVEA,U
	PCNAP	3
	LDY	_.SAVEY,U
	LDD	_.SAVEA,U
	BRA	1$
2$	JMP	[_.RETURN,Y]	WHERE TO RETURN TO


*	AND THE EVER POPULAR COPYRIGHT MESSAGE

	FCC	'JOUST-(C)1982 WILLIAMS ELECTRONICS INC.'

ENDADR_TB12REV3	EQU	*
LENGTH_TB12REV3	EQU	ENDADR_TB12REV3-TSBORG
;	END	SYSV
 
                **** MESSAGE.SRC ****

	NAM	MESSAGE BLOCK OUTPUT ROUTINES AND DATA
********	NLIST
;	INCLUDE RAMDEF.SRC
;	INCLUDE SHRAMDEF.SRC
;	INCLUDE EQU.SRC
;	INCLUDE MESSEQU.SRC
;	INCLUDE MESSEQU2.SRC
;	LIST = List the assembler output listing

*********************************************************
*							*
*	CHARACTER,MESSAGE, & BCD OUTPUT ROUTINE		*
*							*
*	STARTED : MAY 25, 1982	BY : CARY KOLKER	*
*							*
*********************************************************


*	DMA EQUATES	*
* DMA BECAUSE BILL HAS IT ALREADY
*DMA	EQU	$CA00	CONTROL REGISTER OF DMA
CON	EQU	DMA+1	CONSTANT WRITE REGISTER
ORG	EQU	CON+1	ORIGIN (WHERE DATA IS FROM) 2-BYTES
DES	EQU	ORG+2	DESTINATION (WHERE DATA IS TO GO) 2-BYTES
XSIZE	EQU	DES+2	HORIZONTAL SIZE (X-SIZE)
YSIZE	EQU	XSIZE+1	VERTICAL SIZE (Y-SIZE)


	ORG	MESS	START AT THE ADDRESS SPECIFIED ABOVE
	JMP	CHROUT	JUMP TO THE CHARACTER OUTPUT ROUTINE
	JMP	PHROUT	JUMP TO THE PHRASE OUTPUT ROUTINE
	JMP	BCDOUT	JUMP TO THE B.C.D. OUTPUT ROUTINE
	JMP	CHR35	JUMP TO THE CHARACTER 3X5 OUTPUT ROUTINE
	JMP	PHR35	JUMP TO THE PHRASE 3X5 OUTPUT ROUTINE
	JMP	BCD35	JUMP TO THE B.C.D. 3X5 OUTPUT ROUTINE
	JMP	OUTTEXT	JUMP TO THE TEXT 5X7 OUTPUT ROUTINE
	JMP	OUTT35	JUMP TO THE TEXT 3X5 OUTPUT ROUTINE
  IF KeepFontData
	FDB	FONT57	FOR BILL SO HE CAN USE THE NUMBERS
  ENDIF
	JMP	ERTEXT	JUMP TO THE TEXT 3X5 ERASE ROUTINE
	JMP	ERTT35	JUMP TO THE TEXT 5X7 ERASE ROUTINE

	PAGE

*********************************************************
*							*
*	CHROUT - REQUIRES:				*
*		A - CONTAIN THE SELECT CHARACTER	*
*		B - CONTAIN THE COLOR TO WRITE IT IN	*
*		X - CONTAIN THE ADDRESS TO WRITE AT	*
*							*
*			OR				*
*							*
*	CHR35 - REQUIRES THE SAME PARAMETERS		*
*		BUT IS ONLY FOR 3X5 CHARACTERS		*
*							*
*	RESULT IS:					*
*		CC - IS ALTERED SLIGHTLY		*
*		D,Y,U,S,DP - REMAIN UNCHANGED		*
*							*
*	CHROUT - WILL WRITE A CHARACTER TO THE LOCATION	*
* POINTED TO BY X, WITH THE COLOR IN B, SELECT CHARACTER*
* IN A...	NOTE: THIS WILL DO A CONSTANT WRITE OF	*
* COLOR SPEC. WITH ZERO SUPRESS.	TO ERASE CALL	*
* CHROUT WITH A COLOR OF ZERO (WHEN BACKGROUND IS 0)	*
*							*
*********************************************************

;CHR35:
;	PSHS	U,Y,D,CC	SAVE THE CONDITIONS
;	LDY	#FONT35	GET THE FONT TABLE OF THE CHARACTERS
;	ORCC	#$FF	TELL HIM NOT TO DISTURB ME WHILE I WRITE
;	STB	CON	STORE THE COLOR SO WE CAN NOW USE THE D. REG.
;	BSR	OUTPUT	GO TO THE NORMAL ROUTINE
;	PULS	U,Y,D,CC,PC	RESTORE THE CONDITIONS WE HAD WHEN WE ENTERED


;CHROUT:
;	PSHS	U,Y,D,CC	SAVE THE CONDITIONS
;	LDY	#FONT57	GET THE FONT TABLE OF THE CHARACTERS
;	ORCC	#$FF	TELL HIM NOT TO DISTURB ME WHILE I WRITE
;	STB	CON	STORE THE COLOR SO WE CAN NOW USE THE D. REG.
;	BSR	OUTPUT	GO TO THE NORMAL ROUTINE
;	PULS	U,Y,D,CC,PC	RESTORE THE CONDITIONS WE HAD WHEN WE ENTERED


;OUTPUT:
;	STX	DES	STORE THE DESTINATION, SO AS TO USE X AND SAVE 1-CYCLE
;	ASLA		MAKE THE CHARACTER # AN ADDRESS OFFSET
;	LDY	A,Y	GET THE ADDRESS OF THE ACTUAL CHARACTER DATA
;	LDD	,Y++	GET THE X & Y SIZES OF THE CHARACTER
;	EORA	#$04	-----|--FOR THE INVERTED BIT DMA
;	EORB	#$04	-----|
;	STD	XSIZE	STORE THE X & Y SIZES TO THE DMA
;	STY	ORG	STORE THE ORIGIN TO THE DMA
;	LDB	#$1A	READ SERIAL, WRITE BLOCK, CONSTANT SUBB, ZERO SUPPRESS
;	STB	DMA	START THE DMA WITH THE SELECTED OPERATION
;	EORA	#$04	HAVE TO DO IT BECAUSE OF THE INVERTED DMA'S
;	CLRB		MAKE D. HAVING B. A 00
;	LEAX	D,X	MOVE ME TO THE NEXT SPOT
;	RTS

	PAGE

*********************************************************
*							*
*	PHROUT - SAME AS CHROUT EXCEPT:			*
*		A - CONTAINS A PHRASE NUMBER TO OUTPUT	*
*		AT THE ADDRESS SPECIFIED BY THE X REG.	*
*		THIS IS FOR 5X7 CHARACTER OUTPUT ONLY	*
*							*
*			OR				*
*							*
*	PHR35 - SAME AS PHROUT BUT FOR 3X5 CHARACTERS	*
*		ONLY...					*
*							*
*	RESULTS - SAME AS CHROUT			*
*							*
*********************************************************

;PHR35:
;	PSHS	U,Y,D,CC	SAVE THE CONDITIONS
;	LDY	#FONT35	GET THE (3 X 5) FONT TABLE
;	STY	XSAVE	STORE TO XSAVE EVEN IF ITS Y
;	ORCC	#$FF	TELL HIM NOT TO DISTURB ME WHILE I WRITE
;	STB	CON	STORE THE COLOR SO WE CAN NOW USE THE D. REG.
;	BSR	PHROT1	AND GO TO THE NORMAL ROUTINE
;	PULS	U,Y,D,CC,PC	RESTORE THE CONDITIONS WE HAD WHEN WE ENTERED


;PHROUT:
;	PSHS	U,Y,D,CC	SAVE THE CONDITIONS
;	LDY	#FONT57	GET THE (5 X 7) FONT TABLE
;	STY	XSAVE	STORE TO XSAVE EVEN IF ITS Y
;	ORCC	#$FF	TELL HIM NOT TO DISTURB ME WHILE I WRITE
;	STB	CON	STORE THE COLOR SO WE CAN NOW USE THE D. REG.
;	BSR	PHROT1	GO TO THE NORMAL ROUTINE
;	PULS	U,Y,D,CC,PC	RESTORE THE CONDITIONS WE HAD WHEN WE ENTERED


;PHROT1:
;	LDU	#PHRASE	GET THE TABLE OF PHRASES
;	LEAU	A,U	BECAUSE OF SIGNED OFFSETS WE HAVE TO MOVE U AHEAD
*			THEN LOAD OFF OF THE NEXT INSTRUCTION
;	LDU	A,U	MOVE THE TABLE TO THAT POSITION
;1$ 	LDA	,U	GET THE CHARACTER POINTED TO BY Y.
;	LDY	XSAVE	GET THE FONT TABLE
;	JSR	OUTPUT	PUT IT OUT AND USE CHROUT'S RETURN
;	TST	,U+	TEST IF THIS WAS THE LAST CHARACTER OF THE PHRASE
;	BPL	1$	BRANCH IF IT WAS NOT (BIT 7 NOT SET)
;	RTS		RETURN TO THE CALLER


	PAGE

*********************************************************
*							*
*	BCDOUT - SAME AS CHROUT EXECPT:			*
*		A - CONTAINS B.C.D. NUMBER TO OUTPUT	*
*		FOR 5X7 CHARACTER OUTPUT ONLY		*
*							*
*			OR				*
*							*
*	BCD35 - SAME AS CHROUT EXECPT:			*
*		A - CONTAINS B.C.D. NUMBER TO OUTPUT	*
*		FOR 3X5 CHARACTER OUTPUT ONLY		*
*							*
*	RESULTS - NUMBER IS OUTPUTED SAME AS CHROUT	*
*		WITH HIGH NIBBLE WHERE X POINTED	*
*		ALSO IF HIGH NIBBLE IS GREATER THAN	*
*		9 THEN IT IS BLANKED			*
*							*
*********************************************************

;BCD35:
;	PSHS	U,Y,D,CC	SAVE THE CONDITIONS
;	LDY	#FONT35	GET THE (3 X 5) FONT TABLE
;	STY	XSAVE	STORE TO XSAVE EVEN IF ITS Y
;	ORCC	#$FF	TELL HIM NOT TO DISTURB ME WHILE I WRITE
;	STB	CON	STORE THE COLOR SO WE CAN NOW USE THE D. REG.
;	BSR	BCDOT1	GO TO THE NORMAL FOR THE REST OF THE WORK
;	PULS	U,Y,D,CC,PC	RESTORE THE CONDITIONS WE HAD WHEN WE ENTERED


;BCDOUT:
;	PSHS	U,Y,D,CC	SAVE THE CONDITIONS
;	LDY	#FONT57	GET THE (5 X 7) FONT TABLE
;	STY	XSAVE	STORE TO XSAVE EVEN IF ITS Y
;	ORCC	#$FF	TELL HIM NOT TO DISTURB ME WHILE I WRITE
;	STB	CON	STORE THE COLOR SO WE CAN NOW USE THE D. REG.
;	BSR	BCDOT1	GO TO THE NORMAL FOR THE REST OF THE WORK
;	PULS	U,Y,D,CC,PC	RESTORE THE CONDITIONS WE HAD WHEN WE ENTERED


;BCDOT1:
;	STA	ASAVE	SAVE A REG. FOR FUTURE USE
;	LSRA		------|
;	LSRA		------|--WE ONLY WANT THE UPPER NIBBLE
;	LSRA		------|
;	LSRA		------|
;	CMPA	#$0A	CHECK TO SEE IF WE SHOULD BLANK THIS DIGIT
;	BLE	1$
;	LDA	#CSPC	GET A SPACE
;1$	LDY	XSAVE	GET THE FONT TABLE
;	JSR	OUTPUT	AND PUT THE NUMBER OUT
;	LDA	ASAVE	GET THE B.C.D. NUMBER BACK
;	ANDA	#$0F	WE ONLY WANT THE LOWER NIBBLE
;	CMPA	#$0A	CHECK TO SEE IF WE SHOULD BLANK THIS DIGIT
;	BLE	2$
;	LDA	#CSPC	GET A SPACE
;2$	LDY	XSAVE	GET THE FONT TABLE
;	JMP	OUTPUT	PUT IT OUT AND USE CHROUT'S RETURN

* TEXT OUTPUT ROUTINE *

;ERTT35:
;	INC	FLAG	SET THE ERASE FLAG
;	BRA	OUT35A
;OUTT35:
;	CLR	FLAG	CLEAR THE ERASE FLAG
;OUT35A:
;	LDU	#OUTP35	LOAD THE PHRASE OUTPUT ROUTINE
;	BRA	OUTTX1	USE THE CODE OVER

;ERTEXT:
;	INC	FLAG	SET THE ERASE FLAG
;	BRA	OUTTX2
;OUTTEXT:
;	CLR	FLAG	CLEAR THE ERASE FLAG
;OUTTX2:
;	LDU	#OUTPHR	LOAD THE PHRASE OUTPUT ROUTINE
;OUTTX1:
;	LDY	#TXT	GET THE TEXT TABLE
;	LEAY	A,Y	FIND THE ADDRESS OF THE TEXT TO USE FROM THE TABLE
;	LDY	A,Y	---|
;1$	LDX	,Y++	GET THE ADDRESS TO WRITE IT AT
;	LDB	,Y+	GET THE COLOR TO WRITE IT IN
;	TST	FLAG	ARE WE TO ERASE THIS
;	BEQ	2$	BRA= NO SO USE THE COLOR
;	CLRB		HE SAYS WERE TO ERASE IT SO CLEAR THE COLOR
;2$	LDA	,Y	GET THE PHRASE TO USE
;	ANDA	#$7F	GET RID OF THE END OF TEXT FLAG
;	JSR	,U	JSR TO THE SPECIFIED OUTPUT ROUTINE
;	TST	,Y+	CHECK THE END OF TEXT FLAG B7=1 - DONE, B7=0 - NOT DONE
;	BPL	1$	BRA= WERE NOT DONE YET
;	RTS		LET'S GO HOME......


	PAGE

*********************************************************
*							*
*	CHARACTER TABLES:				*
*		5X7 FONT FIRST				*
*		3X5 FONT SECOND				*
*							*
*********************************************************
  IF KeepFontData
FONT57	FDB	L0
	FDB	L1
	FDB	L2
	FDB	L3
	FDB	L4
	FDB	L5
	FDB	L6
	FDB	L7
	FDB	L8
	FDB	L9
	FDB	LSPC
	FDB	LA
	FDB	LB
	FDB	LC
	FDB	LD
	FDB	LE
	FDB	LF
	FDB	LG
	FDB	LH
	FDB	LI
	FDB	LJ
	FDB	LK
	FDB	LL
	FDB	LM
	FDB	LN_MESSAGE
	FDB	LO
	FDB	LP
	FDB	LQ
	FDB	LR
	FDB	LS
	FDB	LT
	FDB	LU
	FDB	LV
	FDB	LW
	FDB	LX
	FDB	LY
	FDB	LZ
	FDB	LBARW
	FDB	LEQU
	FDB	LDSH
	FDB	LQUE
	FDB	LEXC
	FDB	LBRKL
	FDB	LBRKR
	FDB	LSQOT
	FDB	LCMMA
	FDB	LPER
	FDB	LSLSH
	FDB	LAMP
	FDB	LDQOT
	FDB	LCOLON
	FDB	LCUR
	FDB	LCNARW

FONT35	FDB	S0
	FDB	S1
	FDB	S2
	FDB	S3
	FDB	S4
	FDB	S5
	FDB	S6
	FDB	S7
	FDB	S8
	FDB	S9
	FDB	SSPC
	FDB	SA
	FDB	SB
	FDB	SC
	FDB	SD
	FDB	SE
	FDB	SF
	FDB	SG
	FDB	SH
	FDB	SI
	FDB	SJ
	FDB	SK
	FDB	SL
	FDB	SM
	FDB	SN
	FDB	S0
	FDB	SP
	FDB	SQ
	FDB	SR
	FDB	S5
	FDB	ST
	FDB	SU
	FDB	SV
	FDB	SW
	FDB	SX
	FDB	SY
	FDB	SZ
	FDB	SBARW
	FDB	SEQU
	FDB	SDSH
	FDB	SQUE
	FDB	SEXC
	FDB	SBRKL
	FDB	SBRKR
	FDB	SSQOT
	FDB	SCMMA
	FDB	SPER
	FDB	S000
	FDB	SARRW

********	NLIST

L0	FCB	$03,$07		XSIZE,YSIZE
	FCB	$01,$11,$00	CHARACTER '0'
	FCB	$10,$00,$10
	FCB	$10,$01,$10
	FCB	$10,$10,$10
	FCB	$11,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00

L1	FCB	$03,$07
	FCB	$00,$10,$00	CHARACTER '1'
	FCB	$01,$10,$00
	FCB	$00,$10,$00
	FCB	$00,$10,$00
	FCB	$00,$10,$00
	FCB	$00,$10,$00
	FCB	$11,$11,$10

L2	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER '2'
	FCB	$10,$00,$10
	FCB	$00,$00,$10
	FCB	$00,$11,$00
	FCB	$01,$00,$00
	FCB	$10,$00,$10
	FCB	$11,$11,$10


L3	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER '3'
	FCB	$10,$00,$10
	FCB	$00,$00,$10
	FCB	$00,$11,$00
	FCB	$00,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00


L4	FCB	$03,$07
	FCB	$00,$01,$10	CHARACTER '4'
	FCB	$00,$10,$10
	FCB	$01,$00,$10
	FCB	$10,$00,$10
	FCB	$11,$11,$10
	FCB	$00,$00,$10
	FCB	$00,$00,$10


L5	FCB	$03,$07
	FCB	$11,$11,$10	CHARACTER '5'
	FCB	$10,$00,$00
	FCB	$11,$11,$00
	FCB	$00,$00,$10
	FCB	$00,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00


L6	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER '6'
	FCB	$10,$00,$00
	FCB	$10,$11,$00
	FCB	$11,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00


L7	FCB	$03,$07
	FCB	$11,$11,$10	CHARACTER '7'
	FCB	$10,$00,$10
	FCB	$00,$01,$00
	FCB	$00,$10,$00
	FCB	$01,$00,$00
	FCB	$01,$00,$00
	FCB	$01,$00,$00


L8	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER '8'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00


L9	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER '9'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$01,$10
	FCB	$01,$10,$10
	FCB	$00,$00,$10
	FCB	$01,$11,$00

LSPC	FCB	$03,$07
	FCB	$00,$00,$00	CHARACTER ' '
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$00,$00,$00

LA	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'A'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$11,$11,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10


LB	FCB	$03,$07
	FCB	$11,$11,$00	CHARACTER 'B'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$11,$00
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$11,$11,$00


LC	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'C'
	FCB	$10,$00,$10
	FCB	$10,$00,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$10
	FCB	$01,$11,$00


LD	FCB	$03,$07
	FCB	$11,$10,$00	CHARACTER 'D'
	FCB	$10,$01,$00
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$11,$11,$00


LE	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'E'
	FCB	$10,$00,$10
	FCB	$10,$00,$00
	FCB	$11,$11,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$10
	FCB	$01,$11,$00


LF	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'F'
	FCB	$10,$00,$10
	FCB	$10,$00,$00
	FCB	$11,$11,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$00


LG	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'G'
	FCB	$10,$00,$10
	FCB	$10,$00,$00
	FCB	$10,$00,$00
	FCB	$10,$01,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00


LH	FCB	$03,$07
	FCB	$01,$00,$10	CHARACTER 'H'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$11,$11,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10


LI	FCB	$02,$07
	FCB	$11,$10		CHARACTER 'I'
	FCB	$01,$00
	FCB	$01,$00
	FCB	$01,$00
	FCB	$01,$00
	FCB	$01,$00
	FCB	$11,$10


LJ	FCB	$03,$07
	FCB	$00,$01,$10	CHARACTER 'J'
	FCB	$00,$00,$10
	FCB	$00,$00,$10
	FCB	$00,$00,$10
	FCB	$00,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00


LK	FCB	$03,$07
	FCB	$10,$00,$10	CHARACTER 'K'
	FCB	$10,$01,$00
	FCB	$10,$10,$00
	FCB	$11,$00,$00
	FCB	$10,$10,$00
	FCB	$10,$01,$00
	FCB	$10,$00,$10


LL	FCB	$03,$07
	FCB	$01,$00,$00	CHARACTER 'L'
	FCB	$10,$00,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$10
	FCB	$11,$11,$10


LM	FCB	$04,$07
	FCB	$01,$10,$11,$00	CHARACTER 'M'
	FCB	$10,$01,$00,$10
	FCB	$10,$01,$00,$10
	FCB	$10,$01,$00,$10
	FCB	$10,$01,$00,$10
	FCB	$10,$00,$00,$10
	FCB	$01,$00,$01,$00


LN_MESSAGE	FCB	$03,$07
	FCB	$10,$00,$10	CHARACTER 'N'
	FCB	$11,$00,$10
	FCB	$10,$10,$10
	FCB	$10,$10,$10
	FCB	$10,$10,$10
	FCB	$10,$01,$10
	FCB	$10,$00,$10


LO	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'O'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00


LP	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'P'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$11,$11,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$00


LQ	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'Q'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$10,$10
	FCB	$10,$01,$00
	FCB	$01,$10,$10


LR	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'R'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$11,$11,$00
	FCB	$10,$10,$00
	FCB	$10,$01,$00
	FCB	$10,$00,$10

LS	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'S'
	FCB	$10,$00,$10
	FCB	$11,$00,$00
	FCB	$00,$11,$00
	FCB	$00,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00

LT	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER 'T'
	FCB	$10,$10,$10
	FCB	$00,$10,$00
	FCB	$00,$10,$00
	FCB	$00,$10,$00
	FCB	$00,$10,$00
	FCB	$01,$11,$00


LU	FCB	$03,$07
	FCB	$01,$00,$10	CHARACTER 'U'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$11,$00


LV	FCB	$03,$07
	FCB	$01,$00,$10	CHARACTER 'V'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$01,$00
	FCB	$01,$01,$00
	FCB	$00,$10,$00


LW	FCB	$04,$07
	FCB	$01,$00,$01,$00	CHARACTER 'W'
	FCB	$10,$00,$00,$10
	FCB	$10,$00,$00,$10
	FCB	$10,$01,$00,$10
	FCB	$10,$01,$00,$10
	FCB	$10,$01,$00,$10
	FCB	$01,$10,$11,$00


LX	FCB	$03,$07
	FCB	$10,$00,$10	CHARACTER 'X'
	FCB	$10,$00,$10
	FCB	$01,$01,$00
	FCB	$00,$10,$00
	FCB	$01,$01,$00
	FCB	$10,$00,$10
	FCB	$10,$00,$10


LY	FCB	$03,$07
	FCB	$01,$00,$10	CHARACTER 'Y'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$01,$01,$00
	FCB	$00,$10,$00
	FCB	$00,$10,$00
	FCB	$00,$10,$00

LZ	FCB	$03,$07
	FCB	$01,$11,$10	CHARACTER 'Z'
	FCB	$00,$00,$10
	FCB	$00,$01,$00
	FCB	$00,$10,$00
	FCB	$01,$00,$00
	FCB	$10,$00,$00
	FCB	$11,$11,$10

LBARW	FCB	$03,$07
	FCB	$00,$01,$00	CHARACTER '<-'
	FCB	$00,$10,$00
	FCB	$01,$00,$00
	FCB	$11,$11,$10
	FCB	$01,$00,$00
	FCB	$00,$10,$00
	FCB	$00,$01,$00

LEQU	FCB	$03,$05
	FCB	$00,$00,$00	CHARACTER '='
	FCB	$00,$00,$00
	FCB	$11,$11,$10
	FCB	$00,$00,$00
	FCB	$11,$11,$10

LDSH	FCB	$03,$04
	FCB	$00,$00,$00	CHARACTER '-'
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$11,$11,$10

LQUE	FCB	$03,$07
	FCB	$01,$11,$00	CHARACTER '?'
	FCB	$10,$00,$10
	FCB	$10,$00,$10
	FCB	$00,$01,$00
	FCB	$00,$10,$00
	FCB	$00,$00,$00
	FCB	$00,$10,$00

LEXC	FCB	$01,$07
	FCB	$10		CHARACTER '!'
	FCB	$10
	FCB	$10
	FCB	$10
	FCB	$10
	FCB	$00
	FCB	$10

LBRKL	FCB	$02,$07
	FCB	$00,$10		CHARACTER '('
	FCB	$01,$00
	FCB	$10,$00
	FCB	$10,$00
	FCB	$10,$00
	FCB	$01,$00
	FCB	$00,$10

LBRKR	FCB	$02,$07
	FCB	$10,$00		CHARACTER ')'
	FCB	$01,$00
	FCB	$00,$10
	FCB	$00,$10
	FCB	$00,$10
	FCB	$01,$00
	FCB	$10,$00

LSQOT	FCB	$01,$02
	FCB	$10		CHARACTER '''
	FCB	$10

LCMMA	FCB	$01,$07
	FCB	$00		CHARACTER ','
	FCB	$00
	FCB	$00
	FCB	$00
	FCB	$00
	FCB	$10
	FCB	$10

LPER	FCB	$01,$07
	FCB	$00		CHARACTER '.'
	FCB	$00
	FCB	$00
	FCB	$00
	FCB	$00
	FCB	$00
	FCB	$10

LSLSH	FCB	$03,$07
	FCB	$00,$00,$10	CHARACTER '/'
	FCB	$00,$00,$10
	FCB	$00,$01,$00
	FCB	$00,$10,$00
	FCB	$01,$00,$00
	FCB	$10,$00,$00
	FCB	$10,$00,$00

LAMP	FCB	$03,$07
	FCB	$01,$00,$00	CHARACTER '&'
	FCB	$10,$10,$00
	FCB	$10,$10,$00
	FCB	$01,$00,$00
	FCB	$10,$10,$10
	FCB	$10,$01,$00
	FCB	$01,$10,$10

LDQOT	FCB	$02,$02
	FCB	$10,$10		CHARACTER QUOTE
	FCB	$10,$10

LCOLON	FCB	$02,$07
	FCB	$00,$00		CHARACTER ':'
	FCB	$11,$10
	FCB	$11,$10
	FCB	$00,$00
	FCB	$00,$00
	FCB	$11,$10
	FCB	$11,$10

LCUR	FCB	$03,$08
	FCB	$00,$00,$00	CHARACTER '_'
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$00,$00,$00
	FCB	$11,$11,$10

LCNARW	FCB	$03,$07
	FCB	$00,$10,$00	CHARACTER '^'
	FCB	$01,$11,$00
	FCB	$10,$10,$10
	FCB	$00,$10,$00
	FCB	$00,$10,$00
	FCB	$00,$10,$00
	FCB	$00,$10,$00

S0	FCB	$02,$05 	CHARACTER '0 & O'
	FCB	$11,$10
	FCB	$10,$10
	FCB	$10,$10
	FCB	$10,$10
	FCB	$11,$10

S1	FCB	$02,$05
	FCB	$01,$00		CHARACTER '1'
	FCB	$11,$00
	FCB	$01,$00
	FCB	$01,$00
	FCB	$11,$10

S2	FCB	$02,$05
	FCB	$11,$10 	CHARACTER '2'
	FCB	$00,$10
	FCB	$11,$10
	FCB	$10,$00
	FCB	$11,$10

S3	FCB	$02,$05
	FCB	$11,$10 	CHARACTER '3'
	FCB	$00,$10
	FCB	$11,$10
	FCB	$00,$10
	FCB	$11,$10

S4	FCB	$02,$05
	FCB	$10,$10 	CHARACTER '4'
	FCB	$10,$10
	FCB	$11,$10
	FCB	$00,$10
	FCB	$00,$10

S5	FCB	$02,$05
	FCB	$11,$10 	CHARACTER '5 & S'
	FCB	$10,$00
	FCB	$11,$10
	FCB	$00,$10
	FCB	$11,$10

S6	FCB	$02,$05
	FCB	$11,$10 	CHARACTER '6'
	FCB	$10,$00
	FCB	$11,$10
	FCB	$10,$10
	FCB	$11,$10

S7	FCB	$02,$05
	FCB	$11,$10 	CHARACTER '7'
	FCB	$00,$10
	FCB	$01,$00
	FCB	$01,$00
	FCB	$01,$00

S8	FCB	$02,$05
	FCB	$11,$10 	CHARACTER '8'
	FCB	$10,$10
	FCB	$11,$10
	FCB	$10,$10
	FCB	$11,$10

S9	FCB	$02,$05
	FCB	$11,$10 	CHARACTER '9'
	FCB	$10,$10
	FCB	$11,$10
	FCB	$00,$10
	FCB	$00,$10

SSPC	FCB	$02,$05
	FCB	$00,$00 	CHARACTER ' '
	FCB	$00,$00
	FCB	$00,$00
	FCB	$00,$00
	FCB	$00,$00

SA	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'A'
	FCB	$10,$10
	FCB	$11,$10
	FCB	$10,$10
	FCB	$10,$10

SB	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'B'
	FCB	$10,$10
	FCB	$11,$00
	FCB	$10,$10
	FCB	$11,$10

SC	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'C'
	FCB	$10,$00
	FCB	$10,$00
	FCB	$10,$00
	FCB	$11,$10

SD	FCB	$02,$05
	FCB	$11,$00 	CHARACTER 'D'
	FCB	$10,$10
	FCB	$10,$10
	FCB	$10,$10
	FCB	$11,$00

SE	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'E'
	FCB	$10,$00
	FCB	$11,$00
	FCB	$10,$00
	FCB	$11,$10

SF	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'F'
	FCB	$10,$00
	FCB	$11,$00
	FCB	$10,$00
	FCB	$10,$00

SG	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'G'
	FCB	$10,$00
	FCB	$10,$10
	FCB	$10,$10
	FCB	$11,$10

SH	FCB	$02,$05
	FCB	$10,$10 	CHARACTER 'H'
	FCB	$10,$10
	FCB	$11,$10
	FCB	$10,$10
	FCB	$10,$10

SI	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'I'
	FCB	$01,$00
	FCB	$01,$00
	FCB	$01,$00
	FCB	$11,$10

SJ	FCB	$02,$05
	FCB	$00,$10 	CHARACTER 'J'
	FCB	$00,$10
	FCB	$00,$10
	FCB	$10,$10
	FCB	$11,$10

SK	FCB	$02,$05
	FCB	$10,$10 	CHARACTER 'K'
	FCB	$10,$10
	FCB	$11,$00
	FCB	$10,$10
	FCB	$10,$10

SL	FCB	$02,$05
	FCB	$10,$00 	CHARACTER 'L'
	FCB	$10,$00
	FCB	$10,$00
	FCB	$10,$00
	FCB	$11,$10

SM	FCB	$03,$05
	FCB	$11,$11,$10 	CHARACTER 'M'
	FCB	$10,$10,$10
	FCB	$10,$10,$10
	FCB	$10,$00,$10
	FCB	$10,$00,$10

SN	FCB	$02,$05
	FCB	$11,$10		CHARACTER 'N'
	FCB	$10,$10
	FCB	$10,$10
	FCB	$10,$10
	FCB	$10,$10

SP	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'P'
	FCB	$10,$10
	FCB	$11,$10
	FCB	$10,$00
	FCB	$10,$00

SQ	FCB	$02,$05
	FCB	$11,$10		CHARACTER 'Q'
	FCB	$10,$10
	FCB	$10,$10
	FCB	$01,$00
	FCB	$00,$10

SR	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'R'
	FCB	$10,$10
	FCB	$11,$00
	FCB	$10,$10
	FCB	$10,$10


ST	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'T'
	FCB	$01,$00
	FCB	$01,$00
	FCB	$01,$00
	FCB	$01,$00

SU	FCB	$02,$05
	FCB	$10,$10 	CHARACTER 'U'
	FCB	$10,$10
	FCB	$10,$10
	FCB	$10,$10
	FCB	$11,$10

SV	FCB	$02,$05
	FCB	$10,$10		CHARACTER 'V'
	FCB	$10,$10
	FCB	$10,$10
	FCB	$01,$00
	FCB	$01,$00


*	AND THE EVER POPULAR COPYRIGHT MESSAGE

	FCC	' JOUST - COPYRIGHT  (C) 1982 WILLIAMS ELECTRONICS INC. '
	FCC	' ALL RIGHTS RESERVED '


SW	FCB	$03,$05
	FCB	$10,$00,$10 	CHARACTER 'W'
	FCB	$10,$00,$10
	FCB	$10,$10,$10
	FCB	$10,$10,$10
	FCB	$11,$11,$10

SX	FCB	$02,$05
	FCB	$10,$10		CHARACTER 'X'
	FCB	$10,$10
	FCB	$01,$00
	FCB	$10,$10
	FCB	$10,$10

SY	FCB	$02,$05
	FCB	$10,$10 	CHARACTER 'Y'
	FCB	$10,$10
	FCB	$11,$10
	FCB	$01,$00
	FCB	$01,$00

SZ	FCB	$02,$05
	FCB	$11,$10 	CHARACTER 'Z'
	FCB	$00,$10
	FCB	$01,$00
	FCB	$10,$00
	FCB	$11,$10

SBARW	FCB	$02,$05
	FCB	$00,$10 	CHARACTER '<-'
	FCB	$01,$00
	FCB	$11,$10
	FCB	$01,$00
	FCB	$00,$10

SEQU	FCB	$02,$04
	FCB	$00,$00 	CHARACTER '='
	FCB	$11,$10
	FCB	$00,$00
	FCB	$11,$10

SDSH	FCB	$02,$03
	FCB	$00,$00 	CHARACTER '-'
	FCB	$00,$00
	FCB	$11,$10

SQUE	FCB	$02,$05
	FCB	$11,$10 	CHARACTER '?'
	FCB	$00,$10
	FCB	$01,$10
	FCB	$00,$00
	FCB	$01,$00

SEXC	FCB	$01,$05
	FCB	$10		CHARACTER '!'
	FCB	$10
	FCB	$10
	FCB	$00
	FCB	$10

SBRKL	FCB	$02,$05
	FCB	$00,$10 	CHARACTER '('
	FCB	$01,$00
	FCB	$10,$00
	FCB	$01,$00
	FCB	$00,$10

SBRKR	FCB	$02,$05
	FCB	$10,$00 	CHARACTER ')'
	FCB	$01,$00
	FCB	$00,$10
	FCB	$01,$00
	FCB	$10,$00

SSQOT	FCB	$01,$02
	FCB	$10		CHARACTER '''
	FCB	$10

SCMMA	FCB	$01,$05
	FCB	$00		CHARACTER ','
	FCB	$00
	FCB	$00
	FCB	$10
	FCB	$10

SPER	FCB	$01,$05
	FCB	$00	$00 	CHARACTER '.'
	FCB	$00
	FCB	$00
	FCB	$00
	FCB	$10

S000	FCB	$06,$05
	FCB	$11,$10,$11,$10,$11,$10
	FCB	$10,$10,$10,$10,$10,$10
	FCB	$10,$10,$10,$10,$10,$10
	FCB	$10,$10,$10,$10,$10,$10
	FCB	$11,$10,$11,$10,$11,$10

SARRW	FCB	$03,$05
	FCB	$00,$10,$00 	CHARACTER ' '
	FCB	$00,$01,$00
	FCB	$11,$11,$10
	FCB	$00,$01,$00
	FCB	$00,$10,$00
  ENDIF
ENDAD1_MESSAGE	EQU	*		ROUTINE & FONT; LAST ABSOLUTE ADDRESS
LENGT1_MESSAGE	EQU	*-MESS		ROUTINE & FONT; NUMBER OF BYTES THE MODULE USES
;	LIST

********	NLIST

	INCLUDE ./Includes/PHRASE.SRC
;	LIST
ENDADR_MESSAGE	EQU	*		LAST ABSOLUTE ADDRESS
LENGTH_MESSAGE	EQU	*-MESS		NUMBER OF BYTES THE MODULE USES
*
;	IFGT *-GAMORG		OVERFLOW INTO NEXT MODULE?
;	 FCB	$1111		OVERFLOWED INTO NEXT MODULE!!
;	ENDIF
;	END	SYSV

**** JOUSTRV4.SRC.out.out ****
	NAM PFUTZ
;	SUBTTL	WILLIAMS 1982, JOUST GAME
*
******************************************************************
*	WILLIAMS ELECTRONICS 1982
*	ORGINAL GAME NAME; JOUST
*	GAME DESIGNER; JOHN NEWCOMER
*	MAIN PROGRAMMER; BILL PFUTZENREUTER
*	OTHER PROGRAMMERS; CARY KOLKER
*			   KEN LANTZ
*	STARTED PROGRAMMING; FEB 10, 1982
*	FILE: JOUST GAME PROGRAMS
*
;	NLIST
;	INCLUDE	SHRAMDEF.SRC	DONT FORGET STANDARD DEFINITIONS
;;	INCLUDE	SHORTEQU.SRC	CMOS
;	INCLUDE	EQU.SRC	CMOS
;	INCLUDE	MESSEQU.SRC	MESSAGES
; 	LIST
*************************************************************************
*									*
*	PATCHES RAM AREA						*
*									*
*************************************************************************
*
; Moved to the end of SHRAMDEF_CoCo3.SRC to keep RAM use clean and consistant (moves it to B202-B205)
;	ORG	$B300		NEW RAM FOR THE LAVA TROLL
;LAVKLL	RMB	2	TIME LEFT TILL LAVA TROLL GETS REAL STRONG
;CLVGRA	RMB	2	LAVA TROLL CURRENT GRAVITY
*
*
*
*	CONSTANTS
*
HIDE	EQU	$5A		A SIMPLE WAY TO HIDE ASCII DATA
HIDE2	EQU	'0		 (ETC.)
LAND5	EQU	210		START OF LAND
CEILNG	EQU	$0020		CEILING (HIGHEST POSITION) OF GAME)
FLOOR	EQU	$00DF		THE FLOOR (LOWEST POSITION OF GAME)
ELEFT	EQU	-10		EXTREME LEFT SIDE OF WRAP AROUND SCREEN
ERIGHT	EQU	292		EXTREME RIGHT SIDE OF WRAP AROUND SCREEN
MAXVX	EQU	8		MAXIMUM X +- VELOCITY
MAXVY	EQU	$1000		MAXIMUM Y + VELOCITY (FALLING)
MINVY	EQU	$0400		MAXIMUM Y - VELOCITY (RISING)
PLYID	EQU	$09		PLAYERS I.D. WHEN DEAD
EMYID	EQU	$0F		ENEMIES I.D. WHEN DEAD
EGGID	EQU	$02		EGGS I.D. WHEN DEAD
SCRID	EQU	$03		EGGS SCORE I.D.
CATID	EQU	$04		EGGS CAUGHT IN THE AIR SCORE I.D.
PTEID	EQU	$17		PTERODACTYL I.D.
SLOID	EQU	$18		SLOW I.D.
LAVID	EQU	$15
EGGCOL	EQU	$2		CATCH THE EGG IN THE AIR COLOR
PL1	EQU	$5		YELLOW, FOR PLAYER 1
LTBLUE	EQU	$7		 PLAYER 1'S VISOR
PL2	EQU	$7		GREEN, FOR PLAYER 2
YELLOW	EQU	$5		 PLAYER 2'S VISOR
PTC	EQU	$4		RED FOR PTERODACTYL KILLED SCORE
WHI	EQU	$1		WHITE FOR MESSAGES, ETC.
GRY	EQU	$D		LITE GREY FOR TRANSPORTER EFFECT
DKB	EQU	$E		DARK BROWN CLIFF COLOR
LIB	EQU	$8		LIGHT BROWN CLIFF COLOR
MEB	EQU	$8		MEDIUM BROWN CLIFF COLOR

; ATX1	EQU	$11	'WELCOME TO JOUST'
; ATX2	EQU	$12	'TO FLY,'
; *			'REPEATEDLY PRESS 'FLAP' BUTTON'
; ATX3	EQU	$13	'TO SURVIVE A JOUST
; *			'THE HIGHEST LANCE WINS'
; *			'IN A COLLISION'
; ATX4	EQU	$14	'PICK UP THE EGGS'
; *			'BEFORE THEY HATCH'
; ATX5	EQU	$15	'MEET THY ENEMIES'
; ATX6	EQU	$16	'BOUNDER (500)'
; ATX7	EQU	$17	'HUNTER (750)'
; ATX8	EQU	$18	'SHADOW LORD (1500)'
; ATX9	EQU	$19	'PRESS 1 PLAYER TO START'
; *			'OR'
; *			'INSERT ADDITIONAL COIN FOR'
; *			'2 PLAYER EXCITMENT'
; ATX10	EQU	$1A	'READY FOR 2 PLAYER'
; *			'JOUST'
; ATX11	EQU	$1B	'PTERODACTYL BEWARE'
; ATX13	EQU	$1D	'TEMPORARY SAFETY'
; *			'UNTIL A CONTROL IS PRESSED'

*
*	FREE TRANSPORTER TABLE OFFSETS
*
	ORG	$0
FXREV	RMB	1	X POSITION'S WRAP DIRECTION
FXMIN	RMB	2	X PIXEL POSITION MINIMUM
FXMAX	RMB	2	X PIXEL POSITION MAXIMUM
FLEVEL	RMB	2	THIS TRANSPORTERS "IN USE" FLAG ADDRESS
FTLEN	RMB	0	LENGTH OF THIS MINI-TABLE
*
*	TRANSPORTER LOCATION TABLE
*
	ORG	$0
TIMAGE	RMB	2		SOURCE ADDRESS OF IMAGE
TPOSX	  RMB	2		TRANSPORTER X PIXEL POSITION
TPOSY	  RMB	2		TRANSPORTER Y PIXEL POSITION
TCURUSE	RMB	2		TRANSPORTER "IN USE" FLAG ADDRESS
*
*	DECISION BLOCK
*
	ORG	$0
DJOY	  RMB	2	PLAYERS JOYSTICK/DUMB LINE TRACKING ENEMY
DSMART	RMB	2	A SMARTER ENEMY
DSCORE	RMB	2	SCORE RAM LOCATION
DSCLOC	RMB	2	SCORE SCREEN LOCATION
DSCPLY	RMB	2	SCORE'S AREAS IMAGES OF A PERSON SITTING DOWN
DPLYR	  RMB	2	RIDERS IMAGE
DTREFF	RMB	2	TRANSPORTERS COLOR EFFECTS
DCRE	  RMB	2	DECISION TO BE RECREATED BY WHOM
DDEAD 	RMB	2	DEATH WISH ROUTINE (REG.X VICTOR, REG.U LOSER)
DEGGS 	RMB	2	EGG KILLED COUNTER
DSTART	RMB	2	DECISION TO BE ENABLED AFTER TRANSPORTER
DVALUR	RMB	2	SCORE'S ROUTINE TO CALL FOR SCORES VALUE
DGLAD 	RMB	2	GLADIATOR/TEAM WAVE RAM POINTER
DTIME 	RMB	2	ADDRESS OF RUNNING/FLYING NAP TIME
DSNWU 	RMB	2	SOUND OF WINGS GOING UP
DSNWD 	RMB	2	SOUND OF WINGS GOING DOWN
DSNSK 	RMB	2	SOUND OF SKIDDING
DSNSK2	RMB	2	END OF SKIDDING SOUND
DSNRU1	RMB	2	SOUND OF RUNNING (CLIP) (FIRST LEG)
DSNRU2	RMB	2	SOUND OF RUNNING (CLOP) (SECOND LEG)
DSNFAL	RMB	2	SOUND OF FALLING
DSNTREF	RMB	2	KILL SOUND OF TRANSPORTER EFFECT
DSNCRE	RMB	2	THE SOUND OF A TRANSPORTER EFFECT
DCONST	RMB	1	COLOR NIBBLE OF PLAYER
DVALUE	RMB	1	SCORE WHEN KILLED (IN THOUSANDS/HUNDREDS)
DGAMO 	RMB	1	PUT UP PLAYER 1/2 GAME OVER MESSAGE
*
*	PICTURES FRAME NUMBER (FOR PIMAGE,U)
*
	ORG	$0
SKID	RMB	12
STAND	RMB	12
RUN1	RMB	12
RUN2	RMB	12
RUN3	RMB	12
RUN4	RMB	12
FLY1	RMB	12
FLY2	RMB	12
FLY3	RMB	12
*
*	WAVE PROCESS OFFSETS
*
	ORG	PPOSX
PWAVE	RMB	2
PDELWV	RMB	2	TIME LEFT TO DELAY INBETWEEN WAVES
PDELAY	RMB	2	DELAY COUNTER
PINTEL	RMB	2	TEMP STORE AREA FOR INTELLIGENCE
PWRTS	RMB	2	RETURN ADDRESS FOR SOME SUBROUTINES
PWRTS2	RMB	2	RETURN ADDRESS FOR SOME SUBROUTINES
PWPREV	RMB	2	PREVIOUS WAVE CLEAN-UP SUBROUTINE ADDRESS
PWREGD	RMB	2	SAVE AREA FOR REG.D
PWREGY	RMB	2	SAVE AREA FOR REG.Y (EGG WAVE)
PWREGA	RMB	1	SAVE AREA FOR REG.A (EGG WAVE)
PEGGTM	RMB	1	TIME TILL EGGS HATCH FOR EGG WAVE
PBAITN	RMB	1	WORD OFFSET TO TIME TABLE OF NEXT BAITER SEND OFF
PBAITS	RMB	1	START TIME TABLE;TABLE NUMBER TILL NEXT BAITER SEND OFF
PWHCH	RMB	1	IF >0 A PRE-MATURE HATCHING EGG TO BE CREATED
PMSGA	RMB	1	CURRENT MESSAGE BEING DISPLAYED
PMSG0	RMB	4	MSG FOR WAVE 'XX'
PMSG1	RMB	4	MSG NBR & COLOR & SCREEN ADDRESS
PMSG2	RMB	4	MSG NBR & COLOR & SCREEN ADDRESS
PMSG3	RMB	4	MSG NBR & COLOR & SCREEN ADDRESS
PMSG4	RMB	4	MSG NBR & COLOR & SCREEN ADDRESS
PMSG5	RMB	4	MSG NBR & COLOR & SCREEN ADDRESS
;	IFGT	PNBR-*
;	 FCB	$1111	CIA PROCESS BLOCK OVERFLOW!
;	ENDIF
*
*	WAVE OFFSETS
*
	ORG	$0
WBOUND	RMB	0	NBR OF BOUNDERS IN THIS WAVE (UPPER NIBBLE)
WHUNT	RMB	1	NBR OF HUNTERS IN THIS WAVE (LOWER NIBBLE)
WLORD	RMB	0	NBR OF SHADOW LORDS IN THIS WAVE (UPPER NIBBLE)
WPERSUE	RMB	1	NBR OF PERSUERS IN THIS WAVE (LOWER NIBBLE)
WPTEN	RMB	1	NBR OF PTERODACTYLS IN THIS WAVE
WSTATUS	RMB	1	STATUS BYTE FOR THIS WAVE
WLEN	EQU	*
*
*	WAVE STATUS BYTE, BIT DEFINITION
*
WBPER	EQU	$01	IF=1, PERSUE WILL BE INSTANTANIOUSLY
WBJSR0	EQU	$02	THESE 3 BITS ARE A JSR TABLE OFFSET
WBJSR1	EQU	$04	 (0 = AN NOP, OTHERS ARE 2,4,6,8,10,12,14)
WBJSR2	EQU	$08
WBCL1L	EQU	$10	IF=1, CLIFF1L IS DISABLED (OR DESTROYED)
WBCL1R	EQU	$20	IF=1, CLIFF1R IS DISABLED (OR DESTROYED)
WBCL2	EQU	$40	IF=1, CLIFF2 IS DISABLED (OR DESTROYED)
WBCL4	EQU	$80	IF=1, CLIFF4 IS DISABLED (OR DESTROYED)
WBCL1	EQU	WBCL1L+WBCL1R
WBCL12	EQU	WBCL1+WBCL2
WBCL14	EQU	WBCL1+WBCL4
WBCL24	EQU	WBCL4+WBCL2
WBCLS	EQU	WBCL1+WBCL2+WBCL4
*
*	DYNAMIC GAME ADJUST TABLE OFFSETS
*
	ORG	$0
DYWST0	RMB	2	WORD START 0,1,2,3
DYWST1	RMB	2	WORD START 4,5,6
DYWST2	RMB	2	WORD START 7,8,9
DYWEND	RMB	2	WORD END VALUE
DYWTIM	RMB	5	NIBBLE TIMES BEFORE NEXT ADJUSTMENTS
DYWINC	RMB	1	BYTE LENGTH,INCREMENT
DYWLEN	RMB	0
*
;DYWORD	MACRO START1,START2,START3,INCRE,ENDV,TIM1,TIM2,TIM3
DYWORD	MACRO
	 FDB	\1,\2,\3,\5,\6,\7
	 FCB	\8,\4
	ENDM
*
	ORG	GAMORG
*
*	THE PROGRAM'S VECTORS
*
	JMP	ATTRCT		POWER UP VECTOR
	JMP	ATTRC2		RETURN HERE AFTER H.S.T.D.
	FDB	GSTART		GAME START BUTTON MONITOR ROUTINE
	JMP	SCREDIT		DISPLAY CURRENT NUMBER OF CREDITS LEFT
	FDB	SNHIGH		HIGH SCORE TO DATES SOUND
	JMP	HSTD		TEMPORARY GO TO H.S.T.D. PAGE
	JMP	DCREDIT		DISPLAY CREDITS
	JMP	GAMSIM		SIMULATION GAME OR INSTRUCTIONAL PAGE
	JMP	DCRE2		DISPLAY CREDITS (VARIABLE BACKGROUND)
	FDB	NULL		COLOR RAM OF ALL ZEROS
*
*	GAME SIMULATION OR INSTRUCTIONAL PAGE - moved a copy of this to high RAM $F000- area
*
GAMSIM	LDA	#$7F		GAME SIMULATION MODE
	STA	GOVER
	LDA	#3		3 LIVES FOR EACH PLAYER
	STA	NLIVES
	STA	NPLYRS		& 2 PLAYERS
	JMP	SIM1A		START THE PAGE
*
SIM2A	CLR	NSMART		NO-ONE IS SMART IN THIS GAME!
	CLR	WSMART
*
*	GENERATE BACKGROUND LANDING & COLISION TABLES
*
	LDX	#BCKXD1		GENERATE BACKGROUND COLISION TABLE AREA
L003_000:	LDD	,X
	ANDA	#$80
	ANDB	#$80
	STD	,X++
	CMPX	#BCKXD2
	BLO	L003_000
*
	PCNAP	1
	LDX	#LNDXD1		GENERATE BACKGROUND LANDING TABLE AREA
L005_000:	LDA	,X
	ANDA	#$20		FOR NOW, JUST CLIFF 5 LANDS AT ALL POINTS
	STA	,X+
	CMPX	#LNDXD2
	BLO	L005_000
*
PAMSG	EQU	PHORSE		MESSAGE AREAS PHORSE,PRIDER
*
	LDD	#0
	STD	LEVPAS		NO LEVEL PASSES DURING ATTRACT MODE
	STD	PAMSG+0,U	INIT MESSAGE AREAS
	STD	PAMSG+2,U
	LDA	#-1
	STA	CURTR1		BREAK DOWN ALL BUT 1 TRANSPORTER
	STA	CURTR2		 (THE BOTTOM ONE IS OPERATIONAL)
	STA	CURTR3
	JSR	DCREDIT		DISPLAY CREDITS
	LDX	#ATMST-5		START SIMULATION MODE
	STX	PDIST,U
L010_000:	LDX	PDIST,U
	LEAX	5,X
	STX	PDIST,U
	CMPX	#ATMEND
	LBHS	SIM3A		BR=END OF SIMULATION
	LDA	4,X		ERASE OLD MESSAGE
	BEQ	L011_000
	LEAX	PAMSG,U
L012_000:	CMPA	,X+		IS THE MESSAGE IN THIS SPOT?
	BNE	L012_000		 BR=NO
	CLR	,-X		ERASE QUE'D WRITTING
	TSTA
	BLT	>       ;*+2+3+2		!!!!!!!!!BECAREFULL!!!!!!!!!!
	JSR	ETEXT		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	BRA	L011_000		!!!!!!!!!BECAREFULL!!!!!!!!!!
*				!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	ANDA	#$7F		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	JSR	ETEXT35
L011_000:	PCNAP	8
	LDX	PDIST,U
	LDA	2,X		SEND NEW MESSAGE
	BSR	L025_000
	PCNAP	8
	LDX	PDIST,U
	JSR	[,X]
	LDX	PDIST,U
	LDA	3,X		SLEEP TIME
	STA	PJOYT,U
L019_000:	LDA	#60/2/3
	STA	PEGG,U
L020_000:	LDA	PAMSG+0,U
	BSR	L030_000
	PCNAP	2
	LDA	PAMSG+1,U
	BSR	L030_000
	PCNAP	2
	LDA	PAMSG+2,U
	BSR	L030_000
	PCNAP	2
	DEC	PEGG,U
	BNE	L020_000
	DEC	PJOYT,U
	BNE	L019_000
	BRA	L010_000
*
*	PUT MESSAGE IN QUE & WRITE IT
*
L025_000:	BEQ	L040_000
	LEAX	PAMSG,U
	TST	,X+		THIS SLOT EMPTY? !!!!!!!!!BECAREFULL!!!!!!!!!!
	BNE	*-2		 BR=NO		 !!!!!!!!!BECAREFULL!!!!!!!!!!
	STA	,-X		NOW THIS SLOT IS FULL
*
*	WRITE TEXT OR TEXT35
*
L030_000:	BEQ	L040_000		!!!!!!!!!BECAREFULL!!!!!!!!!!
	BLT	>       ;*+2+3		!!!!!!!!!BECAREFULL!!!!!!!!!!
	JMP	TEXT		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*				!!!!!!!!!BECAREFULL!!!!!!!!!!
!
	ANDA	#$7F		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	JMP	TEXT35		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
L040_000:	RTS
*
ATMRTS	RTS
ATMST	FDB	ATMRTS,ATX1*256+2	INTRO MESSAGE
	FCB	0
	FDB	FLYLES,ATX2*256+9	FLYING LESSIONS
	FCB	0
	FDB	DIELES,ATX3*256+7	DYING LESSIONS
	FCB	ATX2
	FDB	EGGLES,ATX4*256+5	EGG LESSION
	FCB	ATX3
	FDB	LAVLES,ATX5*256+2	ENEMY LESSIONS
	FCB	ATX4
	FDB	BOULES,ATX6*256+2	BOUNDER
	FCB	0
	FDB	HUNLES,ATX7*256+2	HUNTER
	FCB	ATX6
	FDB	SHALES,ATX8*256+1	SHADOW LORD
	FCB	ATX7
 	FDB	ATMRTS,1		   ERASE ENEMY LESSIONS AREA
	FCB	ATX5
	FDB	ATMRTS,5		   ERASE SHADOW LORD AREA
	FCB	ATX8
	FDB	ATMRTS,(ATX13+$80)*256+6    ERASE SHADOW LORD AREA
	FCB	0			TRANSPORTER
	FDB	OFFLES,2
	FCB	ATX1
 	FDB	PTELES,ATX11*256+10	PTERODACTYL
	FCB	ATX13+$80
	FDB	ATMRTS,0*256+5
	FCB	ATX11
ATMEND	EQU	*
*
*
*	FLYING LESSIONS
*
FLYLES	LDD	#L1FLY
	LDX	TARPL2
	STD	PJOY,X
	LDA	#128+32
	STA	PJOYT,X
	STA	PEGG,X
	RTS
*
L1FLY	DEC	PJOYT,U
	BPL	L020_001
	INC	PJOYT,U
	LDD	PVELY,U		FLY UP
	CMPD	#-$0040
	BMI	L020_001
L010_001:	LDA	PPOSY+1,U
	CMPA	#AOFFL2-9	FLY UP TO JUST ABOVE LINE TRACK OF 2
	BHI	L040_001
	LDD	#L035_000
	STD	PJOY,U
L020_001:	CLRB
L030_001:	CLRA
	STD	CURJOY
	RTS
*
L035_000:	LDA	PVELY,U
	BMI	L020_001
	BRA	L010_001
*
L040_001:	LDD	#L050_000
	STD	PJOY,U
	LDA	PEGG,U
	LSRA
	LSRA
	INCA
	STA	PJOYT,U
L045_000:	LDB	#1		FLAP WINGS
	BRA	L030_001
*
L050_000:	DEC	PJOYT,U
	BNE	L045_000
	LDA	PEGG,U		RESET FLAP TIMER
	LSRA
	STA	PEGG,U
	STA	PJOYT,U
	LDD	#L1FLY
	STD	PJOY,U
	BRA	L020_001
*
*	DIEING LESSION(S)
*
DIELES	LDX	#P4DEC
	JSR	ATTEMY		ATTRACT MODE ENEMIES
	LDD	#LINET		LINE TRACKING BOUNDAR
	STD	PJOY,Y
	LDD	#ERIGHT-1	X POSITION
	STD	PPOSX,Y
	STD	PCOLX,Y
	LDD	#AOFFL2		Y POSITION
	STD	PPOSY,Y
	STB	PCOLY1,Y
	STB	PCOLY2,Y
	STB	PFACE,Y		FACE LEFT
	LDD	#L2EGG
	LDX	TARPLY
	STD	PJOY,X
	LDA	#$60
	STA	PJOYT,X
	RTS
*
*	START ATTRACT MODES ENEMY
*	 INPUT REG.X - INTELLIGENCE
*	 ADDITIONMAL INPUT; SET "PJOY,Y"
*
ATTEMY	PSHS	X			ENEMIES INTELLIGENCE
	LDU	TARPL2		START ENEMY AFTER COLISION ROUTINE
L009_000:	LDU	PLINK,U
	LDA	PID,U
	CMPA	#$20		OPPCOL COLISION ROUTINE?
	BNE	L009_000		 BR=NO
	LDD	#($80+EMYID)*256+$FF	START THE ENEMY';S PROCESS
	LDX	#STFALL
	JSR	VCUPROC
	LDB	#PPOSX			CLEAR OUT THIS GUYS WORKSPACE
L010_002:	CLR	B,Y
	INCB
	CMPB	#PNBR
	BLO	L010_002
	LDU	PEXEC			RESTORE PROCESS AREA
	PULS	X
	STX	PDECSN,Y
	INC	NENEMY
	INC	NRIDER
	LDD	BUZARD_SHRAM		START HORSE
	STD	PHORSE,Y
	LDD	[DPLYR,X]
	STD	PRIDER,Y
********	CLR	PVELX,Y
********	CLR	PBUMPY,Y
********	CLR	PBUMPX,Y
********	LDD	#0
********	STD	PPICH,Y
********	STD	PPICR,Y
	LDA	#4		MAXIMUM NUMBER OF EGGS TO LAY
	STA	PEGG,Y
********	CLR	PCHASE,Y	NOT CHASING THE PLAYER
	RTS
*
*	EGG LESSION
*
EGGLES	LDD	#L1EGG
	LDX	TARPL2
	STD	PJOY,X
	RTS
*
L1EGG	LDD	PSTATE,U
	BEQ	L015_000
	LDA	PVELX,U
	CMPA	#-4*2	MAXIMUM GOING LEFT SPEED?
	BEQ	L017_000
L010_003:	LDD	#$FF00
L011_001:	STD	CURJOY
	RTS
*
L015_000:	LDA	PVELX,U
	BGT	L016_000
	LDD	#$0101
	BRA	L011_001
*
L016_000:	LDD	#$0000
	BRA	L011_001
*
L017_000:	LDD	#L020_002
	STD	PJOY,U
L020_002:	LDA	PVELX,U
	BMI	L010_003
	LDD	#L030_002
	STD	PJOY,U
L030_002:	LDA	PVELX,U
	BNE	L010_003
	LDA	PFACE,U
	BPL	L010_003
	BRA	L016_000
*
*	LAVA TROLL LESSIONS (REALLY ONLY A WARNING)
*
LAVLES	LDX	#$74DC
	LDD	#MSW19*256+$44		'HOME OF THE'
	JSR	OUTP35
	LDX	#$76E3
	LDD	#MSW20*256+$44		'LAVA TROLL'
	JMP	OUTP35
*
*	BOUNDER LESSION
*
BOULES	LDX	#P4DEC
	JSR	ATTEMY
	LDD	#60*6+8+8
	BRA	EMYLES
*
*	HUNTER LESSION
*
HUNLES	LDX	#P5DEC
	JSR	ATTEMY
	LDD	#60*4+8
	BRA	EMYLES
*
*	SHADOW LORDS LESSION
*
SHALES	LDX	#P6DEC
	JSR	ATTEMY
	LDD	#60*2+8
EMYLES	STD	PDIST,Y
	LDD	#8
	STD	PPOSX,Y
	STD	PCOLX,Y
	LDB	#$D3-1
	STB	PPOSY+1,Y
	STB	PCOLY1,Y
	STB	PCOLY2,Y
********	CLR	PFACE,Y
	LDD	#L1EMY
	STD	PJOY,Y
	RTS
*
L1EMY	LDD	PDIST,U
	ADDD	#-1
	STD	PDIST,U
	BLE	L020_003
	LDA	#1		GO RIGHT
	LDB	PVELX,U
	CMPB	#1*2
	BNE	L010_004
L005_001:	CLRA
L010_004:	CLRB
	STD	CURJOY
	RTS
*
L020_003:	LDX	PDECSN,U
	LDD	DSMART,X		ATTRACT MODE KILL
	STD	PJOY,U
	BRA	L005_001
*
*	REMOVE ENEMIES
*
OFFLES	LDU	PLINK,U
	BEQ	L030_003
	LDA	PID,U
	CMPA	#$80+EMYID
	BNE	OFFLES
	LDD	#AUTOFF
	STD	PJOY,U
	BRA	OFFLES
*
L030_003:	LDU	PEXEC
	RTS
*
*	PTERODACTYL LESSION
*
PTELES	LDU	PLINK,U
	LDX	PLINK,U		FIND END OF LINK
	BNE	PTELES
	LDD	#PTEID*256+$FF
	LDX	#PTERST
	JSR	VCUPROC
	LDU	PEXEC
	RTS
*
L2EGG	LDA	PJOYT,U
	BEQ	L020_004
	DEC	PJOYT,U
	LDA	PVELX,U
	BNE	G2JOY
	LDD	#$0100
L010_005:	STD	CURJOY
	RTS
*
L020_004:	LDA	PVELX,U
	BEQ	G2JOY
	LDA	PFACE,U
	BMI	G2JOY
	LDD	#$FF00
	BRA	L010_005
*
*	GAME SIMULATION PLAYER COMMANDS
*
G2JOY
G1JOY	TFR	U,X
L010_006:	LDX	PLINK,X
	BEQ	L020_005
	LDA	PID,X
	CMPA	#PTEID+$80
	BNE	L010_006
	LDA	PFACE,X		FACE OPPOSITE DIRECTION OF PTERODACTYL
	STA	PFACE,U
L020_005:	LDD	#0
	STD	CURJOY
	RTS
*
*	GAME OVER
*
GAMOVR	LDA	#$80		DISABLE CREDIT DISPLAY PAGE
	STA	CRDCHG
	CLR	GOVER		STATE OF GAME = OVER
	PCNAP	8
	PROCCR	GSTART,$48	START GAME START SWITCH MONITOR
	JSR	DCREDIT		DISPLAY NUMBER OF CREDITS LEFT IN THE GAME
	CLR	WSMART		NO-ONE WILL BE SMART
	LDX	[PDUMMY]
GOVID	LDX	PLINK,X		NEXT PROCESS
	BEQ	GOVWAT
	LDA	PID,X		ALL PROCESSES FLAGGED FOR COLISION DETECT
	BPL	GOVID
	CMPA	#$80+PTEID	IGNORE PTERODACTYLS
	BEQ	GOVID
	LDY	PDECSN,X	INTELLIGENCE IS NOW NILL
L002_000:	LDA	PID,X		CARD THIS PROCESS
*
L003_001:	LDY	DJOY,Y
	STY	PJOY,X
	BRA	GOVID
*
*	GAME OVER MESSAGE
*
GOVERM	LDX	#$3090
	LDD	#256*MSGOVR+WHI*$11	PUT UP GAME OVER MESSAGE
	JMP	OUTPHR
*
GOVWAT	LDA	#11		8*11 OR 88 TICK WAIT
	STA	PRAM,U
L001_001:	JSR	GOVERM
	PCNAP	8		WAIT FOR A WHILE
	DEC	PRAM,U
	BNE	L001_001
	LDX	#HSPP1		CLEAR OUT OLD HIGHLITED H.S.T.D. LETTERS
L002_001:	CLR	,X+
	CMPX	#HSPP22+2
	BLO	L002_001
	JMP	GAMEND		CHECK FOR H.S.T.D.
*
ATTRC2	CLR	CRDCHG		RESET CHANGE IN CREDITS
	LDA	ANYONE		BACK FROM H.S.T.D. CHECKS/ENTRIES
	BEQ	L010_007		 BR=NO-ONE ENTERED THEIR NAME
	LDX	#SNHI2		SOMEONE ENTERED THEIR NAME
	JSR	VSND		 MAKE AN ENTERED NAME SOUND
	BRA	SIM3A
*
*
L010_007:	LDA	#109		8*11 OR 88 TICK WAIT
	STA	PRAM,U
L001_002:	JSR	GOVERM
	PCNAP	8		WAIT FOR A WHILE
	DEC	PRAM,U
	BNE	L001_002
SIM3A	PKILL	$00,$00		KILL EVERYONE
HSTD	LDX	#NULL
	JSR	VDCOLOR		BLANK THE COLOR RAM
	PCNAP	4
	JSR	SCCLR		BLANK THE SCREEN
*
* ATTRACT MODE
*
ATTRCT:
	CLR		GOVER		STATE OF GAME = OVER
	LDA		#$31		ATTRACT MODE PROCESS I.D.
	STA		PID,U

	PROCCR	GSTART,$48	START GAME START BUTTON MONITOR - Loop in attract mode waiting for P1 & P2 start button

	LDA		N2SHIP+1		H.S.T.D. PAGE ALLOWED?
	RORA
	BCC		L020_006		BR=NO, GO TO NEXT MENU
	LDD		#15*60			15 SECONDS OF HIGH SCORE TO DATE PAGE
	STD		PRAM,U
	CLR		PRAM+3,U
	LDX		#HICOLR			HIGH SCORE TO DATE COLORS  - Palette2
	JSR		VDCOLOR			* Moves 16 bytes pointed at X to $A00F-$A01E,Kills Y,A,B,X
	JSR		HIGH				DISPLAY HIGH SCORE TO DATE  which jumps to HIGHPO

	JSR		DCREDIT			DISPLAY NUMBER OF CREDITS IN GAME
L010_008:
	PCNAP	1

    INC      PaletteSlot2_Lookup                                        * Increment the position in the match Joust colour table
    LDB      PaletteSlot2_Lookup                                        * B = colour index pointer
    LDX      #Joust_Colour_Match                                        * X= start of lookup table
    LDB      B,X                                                        * B now has CoCo 3 version of Joust's colour

		STB			RAMCOL+2												*	was INC		RAMCOL+2	CONTINUALLY CHANGE 1 COLOR WORD

	LDD		PRAM,U
	BEQ	L020_006
	ADDD	#-1
	STD	PRAM,U
	BRA	L010_008
*
L020_006:
  JSR	READEM   * Read player controls Player 1 bits are in A and Player 2 bits is in B
	SUBD	#$0201 * does P1 = Joy Right and P2 = Joy Left?
	BNE	L030_004
	LDA	#60
	STA	PRAM+3,U
	BRA	L010_008
*
L030_004:	DEC	PRAM+3,U
	BGT	L010_008
	PROCCR	HSTDBK,$49	GO BACK TO H.S.T.D. PAGE MONITOR
	JMP	$D000
*
*	GO BACK TO H.S.T.D. PAGE
*
HSTDBK	PCNAP	5
  JSR	READEM   * Read player controls Player 1 bits are in A and Player 2 bits is in B
	SUBD	#$0201		RIGHT1,LEFT2
	BNE	HSTDBK
	JMP	SIM3A																*** Was a BRA SIM3A
*
* Colour palette for the High scores screen
HICOLR:
Palette2:
;         CoCo  Joust
    FCB    $00  ;$00                                  ;62E9	BACKGROUND
    FCB    $24  ;$07                                  ;62EA 160	LT GREEN TOP BUZZARDS
    FCB    $09  ;$C0                                  ;62EB CENTER SECTION
    FCB    $25  ;$46                                  ;62EC 240	BOTTOM CHAMPIONS
    FCB    $09  ;$C0                                  ;62ED
    FCB    $36  ;$3F                                  ;62EE YELLOW PLAYER 1 ON H.S.T.D. TABLE
    FCB    $09  ;$C0                                  ;62EF
    FCB    $19  ;$E8                                  ;62F0 LT BLUE PLAYER 2 ON H.S.T.D. TABLE
;	FCB	@000		BACKGROUND
;	FCB	@007	160	LT GREEN TOP BUZZARDS
;	FCB	@300		CENTER SECTION
;	FCB	@106	240	BOTTOM CHAMPIONS
;	FCB	@300
;	FCB	@077		YELLOW PLAYER 1 ON H.S.T.D. TABLE
;	FCB	@300
;	FCB	@350		LT BLUE PLAYER 2 ON H.S.T.D. TABLE


NULL	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
	FCB	@000
*
*	 MARKETING MESSAGES
*
MARKET	PCNAP	4		EMPTY BUFFERS
	LDA	#30		TIME OUT (IN SECONDS) FOR THIS MESSAGE PAGE
	STA	PRAM,U
	JSR	SCCLR		CLEAR THE SCREEN
	LDX	VCOLOR1		LOAD COLOR TABLE
	JSR	VDCOLOR
	JSR	DCREDIT		DISPLAY CREDITS
	LDA	#ATX9		ASSUME CREDITS FOR 1 PLAYER
	LDB	CREDIT		CHECK HOW MANY CREDITS THERE ARE
	DECB
	BEQ	L010_009		 BR=1 CREDIT
	LDA	#ATX10		O.K. TO PLAY TWO PLAYERS
L010_009:	JSR	TEXT
L020_007:	PCNAP	60
	DEC	PRAM,U		WATCH FOR TIME OUT
	BNE	L020_007
	JMP	HSTD
*
*	SOUND OF INCREMENTING NUMBER OF CREDITS
*	 (TO BE DISPLAYED LATER)
*
SCREDIT	LDX	#SNCRED		MAKE A CREDITS SOUND
	JSR	VSND
	INC	CRDCHG		INDICATE A COIN CHANGED CREDITS
*
*	DISPLAY NUMBER OF CREDITS
*
DCREDIT:
	LDY	#$1200		CLEAR CREIDT AREA		;6336    108E1200     * Y_MSB=BlitterExecuteMode=$12,Y_LSB=BlitterMask=$00
DCRE2:
	LDD	#$37E2												;633A    CC37E2       * Destination address on screen
;	LDX	#CRDLEN												;633D    8E6357       * Address of width & Height (Down at the end of this routine $6357=$1F09=$1B0D= 27 Wide & 13 High)
;	JSR	GENDMA												;6340    BD8793       * Execute Blitter, D=DestinationAddress, Data @ X is the Width & Height,X++ then is the source Address, B=blitter mask
	JSR	Do_Blitter_01									* BSR down below to Draw black box on screen behind the word Credits
	LDD	#MSCRED*256+$22								;6343    CC5022       * A=message #50="CREDITS", B = mask (Colour=22)
	LDX	#$38E3		SCREEN ADDRESS			;6346    8E38E3       * Destination address on screen
	JSR	OUTPHR												;6349    BD4A53       Writes the message Big Font pointed at by A on screen at X
	LDA	CREDIT		REMEMBER LAST CREDIT VALUE	;634C    96F2         Number of credits also copied to CMOS at 42F5
	BITA	#$F0		SURPRESS LEADING ZERO				;634E    85F0         * Test is the # of credits is double digits
	BNE	L001_003											;6350    2602         * If not skip ahead
	ORA	#$F0													;6352    8AF0         * make left nibble = $F which I guess will be a blank when writing BCD value in A
L001_003:
	JMP	OUTBCD		DISPLAY NBR					;6354    7E4A56       Display BCD number in A on screen at X Big font
*
CRDLEN	FDB	$1F09

*
*	MONITOR THE GAME START SWITCHES
*
GSTART	PKILL	$48,$FF		STOP COPIES OF ITSELF
L004_001:	PCNAP	1
	LDA	CRDCHG		HAVE CREDITS CHANGED?
	BLE	L041_000		 BR=NO
	CLR	CRDCHG
	PKILL	$00,$00		STOP ALL OTHER ATTRACT MODE PROCESSES
	PROCCR	MARKET,$30
L041_000:
  LDB	WPIAA_P1
	ANDB	#$30		PLAYER 1 OR 2 PRESSED?
	BEQ	L004_001		 BR=NO
	LDX	#COINSL		INFINITE FREE PLAY??
	JSR	RCMSA
	CMPA	#$09		 (CODE FOR FREE PLAYS)
	BEQ	L001_004		 BR=YES
	LDA	CREDIT		ANY CREDITS LEFT?
	BEQ	L004_001		 BR=NO
	CMPA	#$02		ENOUGH FOR A 2 PLAYER GAME?
	BHS	L003_002
	BITB	#$10		ONLY CREDITS FOR1 PLAYER, A 2 PLAYER GAME?
	BNE	L004_001		 BR=YES, IGNORE REQUEST
L003_002:	BITB	#$10		2 PLAYER GAME?
	BEQ	L002_002		 BR=NO, MUST BE 1 PLAYER GAME
	ADDA	#$99
	DAA
L002_002:	ADDA	#$99			1 LESS CREDIT
	DAA
	STA	CREDIT		NEW CREDITS LEFT
	LDX	#CREDST		REMEMBER IN CMOS FOR FUTURE POWER-UP
	JSR	WCMSA
L001_004:	LDA	#8		ASSUME BOOKS SINGLE PLAYER START
	CLR	NPLYRS		ASSUME 1 PLAYER GAME
	BITB	#$10		2 PLAYER GAME
	BEQ	GAM1P		 BR=1 PLAYER GAME
	LDA	#9		ASSUME BOOKS DUAL PLAYER START
	INC	NPLYRS		2 PLAYER GAME
	LDB	#10		BOOKS, NBR OF CREDITS STARTED (TOTAL 2)
	JSR	AUDIT1
GAM1P	TFR	A,B		GET AUDIT OFFSET IN REG.B
	JSR	AUDIT1
	LDB	#10		BOOKS, NBR OF CREDITS STARTED
	JSR	AUDIT1
	LDX	#NSHIP		NUMBER OF LIVES FOR THE PLAYER(S)
	JSR	RCMSB
	JSR	BCDHX		BCD TO HEX CONVERT
	STB	NLIVES
	CLR	GOVER		RESET TO TRUE GAME OVER
	PKILL	$00,$00		KILL EVERYONE
	LDA	#$30
	STA	PID,U		LEDGE REFRESH ROUTINE WILL BE ID=30
	LDX	#SNGS		GAME STARTING SOUND
	JSR	VSND

SIM1A	PCNAP	4		WAIT UNTIL BUFFERS ARE EMPTY
	JSR	SCCLR          * CLEAR SCREEN
	LDX	VCOLOR1		LOAD COLOR TABLE
	JSR	VDCOLOR
	LDU	#MSCOP2		ALTERNATE HIDDEN COPYRIGHT MESSAGE
	LDX	,U++		START LOCATION
	LDD	,U++		START LETTER & COLOR
L080_000:	SUBA	#HIDE2
	EORA	#HIDE
	JSR	OUTCHR
	LDA	,U+
	BNE	L080_000
*
*	INITILIZE SOME VARIABLES
*
	LDU	PEXEC
	LDX	#SPLY1		CLEAR PLAYER 1 & 2 SCORE AREA
L010_010:	CLR	,X+
	CMPX	#SPLY2+10
	BLO	L010_010
********	LDD	#0
********	STD	SPLY1+0		CLEAR SCORE, PLAYER 1
********	STD	SPLY1+2
********	STD	SPLY1+4		CLEAR NUMBER OF LIVES A PLAYER HAS
********	STD	SPLY1+6		CLEAR AUX NBR OF LIVES & LEVPAS OFFSET
	CLR	EGGS1		RESET NUMBER OF EGG KILLED
********	STD	SPLY2+0		CLEAR SCORE, PLAYER 2
********	STD	SPLY2+2
********	STD	SPLY2+4		CLEAR NUMBER OF LIVES A PLAYER HAS
********	STD	SPLY2+6		CLEAR AUX NBR OF LIVES & LEVPAS OFFSET
	CLR	EGGS2		RESET NUMBER OF EGG KILLED
*
	CLR	NRIDER		RESET NUMBER OF RIDERS IN THE GAME
	LDX	#REPLAY		READ CMOS LEVEL PASS
	JSR	RCMSB
	CLRA			JUSTIFY IT FOR SCOREING BOUNDS
	ASLB
	ROLA
	ASLB
	ROLA
	ASLB
	ROLA
	ASLB
	ROLA
	STD	LEVPAS		SAVE IT FOR FUTURE USE
	STD	SPLY1+8		SET LEVEL TO PASS FOR PLAYER 1
	STD	SPLY2+8		SET LEVEL TO PASS FOR PLAYER 2
*
	LDX	#GA1		GET GAME ADJUST
	JSR	RCMSA
	CLR	,-S		EXTRA RAM FOR START OFFSET
	CMPA	#3		GAME ADJUST 0,1,2,3 ?
	BLS	L001_005		 BR=NO
	INC	,S
	CMPA	#6		GAME ADJUST 4,5,6?
	BLS	L001_005		 BR=NO
	INC	,S
L001_005:	LDX	#DYTBL		ROM INITILIZATION
	LDY	#DYNADJ		RAM AREA
L002_003:	LDB	,S		START VALUE OFFSET
	ASLB
	LDD	B,X
	STD	,Y
	LDA	#2
	STA	2,Y
	LEAX	DYWLEN,X
	LEAY	3,Y
	CMPX	#DYEND
	BLO	L002_003
*
	LDA	#4		INITILIZE GRAVITY
	STA	GRAV
	LDA	#3
	STA	TBRIDG    ;TBRIDGE		WAVE NBR TO DESTROY BRIDGE
	LDA	#1		NUMBER OF WAVES AFTER BRIDGE DESTROY TILL TROLL
	STA	TTROLL		 COMES OUT IN THE OPEN
	STA	EMYTIM		ENEMY SLOW DOWN TIME
	LDA	#90		1 1/2 SECOND OF DELAY BEFORE TARGETING PLAYER
	STA	TARTIM
	CLR	LAVNBR		NO LAVA TROLLS IN BEGINNING
	LDA	#$EA		START LEVEL OF LAVA FOR BUBBLING LAVA ROUTINE
	STA	SAFRAM
	LDA	#13		NARROWER TRANSPORTER SAFTY ZONE 2ND PTE WAVE
	STA	TRSMALL
	LDD	#0
	STD	CURTR1		FREE UP ALL TRANSPORTERS
	STD	CURTR3		 TR1 TO TR4
	STD	TARPLY		RESET TARGETED PLAYER(S)
	STD	TARPL2
	STD	NPSERV		& LPSERV, RESET TRANSPORTER "TAKE A NUMBER"
	STD	NESERV		& LESERV, RESET TRANSPORTER "TAKE A NUMBER"
	CLR	WPTER		PRESENT NBR OF PTERODACTYLS OF PTERO-WAVE
	CLR	NSMART		NO ONE WAS SMART
	CLR	NENEMY		NO ENEMIES ARE ON THE SCREEN
*
*	GENERATE BACKGROUND LANDING & COLISION TABLES
*
	LDX	#BCKXD1		GENERATE BACKGROUND COLISION TABLE AREA
BCKXUP	LDD	BCKXS1-BCKXD1,X
	STD	,X++
	CMPX	#BCKXD2
	BLO	BCKXUP
*
	PCNAP	1
	LDX	#LNDXD1		GENERATE BACKGROUND LANDING TABLE AREA
LNDXUP	LDA	LNDXS1-LNDXD1,X
	ORA	#$20		FOR NOW, CLIFF 5 LANDS AT ALL POINTS
	STA	,X+
	CMPX	#LNDXD2
	BLO	LNDXUP
*
*	PUT UP THE BACKGROUND
*
BCKUP
	JSR	VNEWCL5		UN-COMPACT CLIF5
	PCNAP	1
;	LDY	#BRIDGE		BRIDGE THE LAVA
;	JSR	BCKYUP
;	LDD	,Y		NEW DMA CONTROL INFORMATION
;	STD	,X
;	LDY	#BRIDG2		2ND HALF OF THE BRIDGE FOR THE LAVA
;	JSR	BCKYUP
;	LDD	,Y		NEW DMA CONTROL INFORMATION
;	STD	,X

  JSR   DrawLavaBridge   * go draw bridge with my code since my blitter code wont handle blocks this size

	LDA	GOVER		GAME SIMULATION?
	BGT	L020_008		 BR=YES
	LDU	#CLIF1L_SHRAM		PUT UP ALL OF THE BACKGROUND MANNUALLY
L010_011:	LDY	,U++		WRITE THIS BCKGND BLOCK
	JSR	BCKYUP		WRITE THIS OBJECT
	CMPU	#CLIF5_SHRAM
	BLO	L010_011
*
	PCNAP	2		ALLOW CLIFFS TO APPEAR UPON SCREEN
	DEC	GOVER		STATE OF GAME = STARTING THE GAME (.NE.)
	SECCR	CIA,$11		CIA WILL BE I.D $11
L020_008:	SECCR	OPPCOL,$20	CREATE OPPONENTS COLISION DETECT ROUTINE
*
	PROCCR	PLAYR,PLYID	CREATE PLAYER #1
	LDD	OSTRICH_SHRAM		START HORSE (OSTRICH)
	STD	PHORSE,Y
	CLR	PFACE,Y		PLAYER IS FACING RIGHT
	LDX	#100		START X POSITION
	STX	PPOSX,Y
	LDX	#G1DEC		ASSUME GAME SIMULATION
	LDA	GOVER		GAME SIMULATION?
	BGT	L030_005		 BR=YES
	LDX	#P1DEC
L030_005:	STX	PDECSN,Y	PLAYER 1'S JOYSTICK
	LDA	NLIVES		NBR OF LIVES FOR THIS PLAYER
	JSR	PLYRUP
	LDA	NPLYRS		CREATE PLAYER #2?
	BEQ	NOPLY2		 BR=NO
	PROCCR	PLAYR,PLYID	CREATE PLAYER #2
	LDD	STORK_SHRAM		START HORSE
	STD	PHORSE,Y
	LDB	#$FF		OSTRICH & RIDER ARE FACEING LEFT
	STB	PFACE,Y
	LDX	#200		START X POSITION
	STX	PPOSX,Y
	LDX	#G2DEC		ASSUME GAME SIMULATION
	LDA	GOVER		GAME SIMULATION?
	BGT	L040_002		 BR=YES
	LDX	#P2DEC
L040_002:	STX	PDECSN,Y	PLAYER 2'S JOYSTICK
	LDA	NLIVES		NBR OF LIVES FOR THIS PLAYER
	JSR	PLYRUP
	PCNAP	8
NOPLY2	PROCCR	PLYCOL,$20	CREATE PLAYERS COLISION DETECT ROUTINE
	LDX	VLAVA		START LAVA PROCESS
	LDD	#$68FF		I.D. $68 SECONDARY PROCESS
	JSR	VCUPROC		 CREATE THE LAVA BUBBLING PROCESS
	SECCR	KZAP,$23
	SECCR	SCODSP,$21	PLAYER 1'S SCORE UPDATE
	LDD	#SCRPL1
	STD	PRAM,Y
	LDA	NPLYRS		2 PLAYER GAME?
	BEQ	NOSPL2		 BR=NO
	SECCR	SCODSP,$22	PLAYER 2'S SCORE UPDATE
	LDD	#SCRPL2
	STD	PRAM,Y
NOSPL2	EQU	*
	LDA	GOVER		GAME SIMULATION?
	LBGT	SIM2A		 BR=YES
	PCNAP	2
	JSR	JZAP
	PROCCR	THORSE,$6F
*
*	REFRESH BACKGROUND LEDGES
*
	LDA	#$FF		SECONDARY PROCESS
	STA	PID,U
LEDGE	CLR	PRAM,U		START IMAGE TO REFRESH(IF ANY)
	CLR	PRAM+2,U	START CLIFF BIT
	CLR	BCKRFS		NOTHING TO REFRESH IN THE BEGINNING
NOBCKR	PCNAP	2
	LDA	BCKRFS		ANY BACKGROUND TO REFRESH?
	BEQ	NOBCKR
*
	LDD	PRAM+1,U	GET CLIFF OFFSET (REG.A), & CLIFF BIT REG.B
BCKNBT	ADDA	#2		NEXT CLIFF
	ASLB
	BNE	BCKN0
	INCB
	CLRA
BCKN0	BITB	BCKRFS		THIS CLIFF BIT TO REFRESH??
	BEQ	BCKNBT		 BR=NO
	STD	PRAM+1,U	SAVE NEW CLIFF BIT & OFFSET BEING REFRESHED
	EORB	BCKRFS
	STB	BCKRFS		TURN THIS CLIFFS REFRESH BIT OFF
	LDY	[PRAM,U]	GET CLIFF ADDRESS
	LEAY	2+6-2,Y		POINT PAST COLISION DETECT & FULL OBJ SIZE
	BSR	BCKYUP		WRITE THIS OBJECT
	LDB	PRAM+2,U	GET REFRESHEING BIT
	BPL	NOBCKR		BR=NOT REFRESHING CLIFF5 (THE BIG ONE)
	PCNAP	1
	LDY	CLIF5_SHRAM		GET CLIFF5 LEFT SIDED ADDRESS
	LEAY	2+6-2+6,Y	POINT PAST COLISION DETECT & FULL OBJ SIZE
	BSR	BCKYUP		WRITE THIS OBJECT
	PCNAP	1
	LDY	CLIF5_SHRAM		GET CLIF5 RIGHT SIDED ADDRESS
	LEAY	2+6-2+6+6,Y	POINT PAST COLISION DETECT & FULL OBJ SIZE
	BSR	BCKYUP		WRITE THIS OBJECT
	BRA	NOBCKR		LOOP FOREVER
*
*	PUT UP BACKGROUND, POINTED TO BY REG.Y  (WRITE THIS Sprite OBJECT)
*
BCKYUP:
	LDB	WCLENY,Y		CALC LOWEST POINT
	EORB	#$04				;!WDMAFIX
	ADDB	WCY,Y
	JSR	VWR1ALL
	LDA	#$0A		WRITE THE BACKGROUND OBJECT
	STA	WCDMA,X
	LDD	WCDEST,Y
	STD	WCDEST,X
	LDD	WCSRC,Y
	STD	WCSRC,X
	LDD	WCLEN,Y
	STD	WCLEN,X
	RTS
*
BRIDGE	FDB	$1200+LIB*$11,0,$00D3,$1F07 			;$1B03!XDMAFIX
BRIDG2	FDB	$1200+LIB*$11,0,$78D3,$1A07				;$1E03!XDMAFIX
*
*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE
*
	FCC	'JOUST (C) 1982 WILLIAMS ELECTRONICS '
	FCB	$20	A DO NOTHING
	FCB	$99	FUDGE TO MAKE THIS PROM ($6000) ADD UP TO $85
	FCB	$67	FUDGE FOR LZAPPER TO SUM CORRECTLY
	FCB	$09	FUDGE TO ADJUST BACK TO THE $6000 SUM OF $85
*
*	PTERODACTYL INTELLIGENCE
*
PTERO	DEC	PPVELX,U	TIME TO ZOOM IN ON THE PLAYER?
	BNE	L047_000		 BR=NO
	LDA	PFEET,U
	STA	PPVELX,U	INITIALIZE NEXT LOOK-AT-PLAYER TIME
	CLR	PRDIR,U		ASSUME NO SELECTED PLAYER
	JSR	SELPLY		SELECT TARGETED PLAYER
	LBEQ	PTELEV		 BR=NO PLAYERS AVAILABLE
	LDA	PFEET,U
	CMPA	#15		MINIMUM TIME TO LOOK-AT-PLAYER
	BLS	L048_000
	DEC	PFEET,U
L048_000:	LDD	PPOSX,X		SAVE HIS CO-ORDINATES
	STD	PDIST,U
	LDB	PPOSY+1,X
	CMPB	#$D3-1		NEVER SEEK HIM BELOW CLIFF5
	BLO	L098_000
	LDB	#$D3-1
L098_000:	SUBB	#5		ADJUSTMENT FOR CENTER OF BIRD/PTERODACTYL
	JSR	PATCH4		PTERODACTYL BETTER KILLER (AIM LOWER)
********	STB	PRDIR,U		X=PDIST, Y=PRDIR
L047_000:	LDB	PRDIR,U		GET VICTIMS Y POSITION
	BEQ	PTELEV		 BR=NO VICTIM
	CMPB	PPOSY+1,U
	LBLO	PTEUP		 BR=GO UP
	SUBB	#15		PART OF PATCH4 (ALSO AIM HIGHER)
********	SUBB	#15
	CMPB	PPOSY+1,U
	LBHI	PTEDN		 BR=GO DOWN
	LDA	#128		ATTACK SEQUENCE TIME-OUT TIME.
	JSR	PATCH5		PTERODACTYL, SLOW DOWN TO KILL PLAYER
********	STA	PEGG,U
	LDX	#SNPTE		PTERODACTYL CALL
	JSR	VSND
	LDD	#PTEATK
	STD	PJOY,U
*
PTEATK
	JSR	PATCH9
********	DEC	PEGG,U		ATTACK SEQUENCE TIME OUT?
	BEQ	L003_003		 BR=YES
	LDD	PDIST,U		WITHIN ATTACK X WINDOW?
	SUBD	PPOSX,U
	BMI	L001_006
	SUBD	#13
	BPL	L002_004		 BR=NO
	BRA	L003_003		YES, SELECT NEXT VICTUM
L001_006:	ADDD	#13
	BMI	L002_004		 BR=NO
L003_003:	LDD	#PTERO		HERE IF ENDING ATTACK SEQUENCE
	STD	PJOY,U
	CLR	PRDIR,U		NO LONGER CHASE AFTER PLAYER
	CLRB
	JMP	PTEDIR
*
L002_004:	LDA	PIMAGE,U	ATTACKE FRAME?
	CMPA	#FLY3-FLY1
	BNE	L020_009
	LDA	#10
	STA	PACCX,U		STAY ON ATTACK FRAME
L020_009:	LDA	PPOSY+1,U	STAY ON CENTER LINE, (OF THE PLAYER)
	CMPA	PRDIR,U
	BEQ	PTEALE		ATTACKING, LEVEL FLIGHT
	BLO	PTEADN		ATTACKING, GO DOWN
	LDA	PVELY,U		NEED TO GO UP, ALREADY GOING UP?
	BMI	L021_000		 BR=YES, NOT TIME TO GO UP YET
	LDD	PVELY,U
	SUBD	#$0010
	STD	PVELY,U		GO UP
L021_000:	BRA	PTEDIR
*
PTEADN	LDA	PVELY,U		NEED TO GO DOWN, ALREADY GOING DOWN?
	BGT	PTEDIR		 BR=YES, NOT TIME TO GO DOWN YET
	LDD	PVELY,U
	ADDD	#$0010
	STD	PVELY,U		GO DOWN
	BRA	PTEDIR
*
PTEALE	CLR	PVELY,U		NO NEED TO GO UP/DOWN
	BRA	PTEDIR
*
PTELEV	LDA	#AOFFL1-2	ASSUME NEAR TRACKING LINE 1
	LDB	PPOSY+1,U
	CMPB	#AOFFL1+AOFFUD
	BLO	L010_012		 BR=TRACK ON LINE 1
	LDA	#AOFFL2-2	ASSUME NEAR TRACKING LINE 2
	CMPB	#AOFFL2+AOFFUD
	JSR	PATCH6		ZOOM INBETWEEN  CLIF3U & CLIF3L,CLIF3R
	NOP
********	BLO	10$		 BR=TRACK ON LINE 2
********	LDA	#AOFFL3-2	ASSUME NEAR TRACKING LINE 3
L010_012:	SUBA	PPOSY+1,U	ABOVE OR BELOW LINE?
	BEQ	L030_006
	BMI	L020_010		BR=ABOVE THE LINE
	LDD	PVELY,U
	CMPD	#$0100
	BGE	PTEDIS
	ADDD	#$0020
	STD	PVELY,U
	BRA	PTEDIS
*
L020_010:	LDD	PVELY,U
	CMPD	#-$0100
	BLE	PTEDIS
	SUBD	#$0020
	STD	PVELY,U
	BRA	PTEDIS
*
L030_006:	LDD	PVELY,U
	ASRA
	RORB
	ASRA
	RORB
	STD	PVELY,U
*
PTEDIS	TST	PBUMPX,U
	BEQ	L030_007
	BMI	L020_011
	LDA	#4
	BRA	L010_013
*
L030_007:	LDA	#4		ASSUME GOING RIGHT
	TST	PVELX,U		MAINTAIN SLOWER VELOCITY!
	BPL	L010_013		 BR=GOING RIGHT
L020_011:	LDA	#-4		REALLY GOING LEFT
L010_013:	STA	PVELX,U		SPEED
	STA	PFACE,U		 & FACING DIRECTION
	LDD	#0
	STD	CURJOY
	RTS
*
PTEDIR	LDA	PFACE,U		FLY IN DIRECTION OF FACING
	STA	PVELX,U
	TST	PBUMPX,U
	BEQ	L030_008
	BMI	L020_012
	LDA	#4*2
	BRA	L010_014
*
L030_008:	LDA	#4*2		ASSUME GOING RIGHT
	TST	PVELX,U		MAINTAIN SLOWER VELOCITY!
	BPL	L010_014		 BR=GOING RIGHT
L020_012:	LDA	#-4*2		REALLY GOING LEFT
L010_014:	STA	PVELX,U		SPEED
	STA	PFACE,U		 & FACING DIRECTION
	LDD	#0
	STD	CURJOY
	RTS
*
PTEUP	LDD	PVELY,U
	CMPD	#-$00C0
	BLE	PTEDN2
	ADDD	#-$0040		DO NOT GO UP TOO FAST
	STD	PVELY,U		DO NOT GO UP TOO FAST
PTEDN2	LDD	#PTEUP2
	STD	PJOY,U
	LDA	#2		WING DOWN TIME
	STA	PJOYT,U
	BRA	PTEDIS
*
PTEUP2	DEC	PJOYT,U
	BGT	PTEDIS
	LDD	#PTERO
	STD	PJOY,U
	BRA	PTEDIS
*
PTEDN	LDD	PVELY,U
	CMPD	#$0100
	BGE	PTEDN2
	ADDD	#$0010
	STD	PVELY,U		 DO NOT GO DOWN TOO FAST
	BRA	PTEDN2
*
*	AUTO MATIC REMOVAL OF PTERODACTYL
*
PTEOFF	LDD	PPOSX,U		OFF SCREEN?
	CMPD	#ELEFT+3
	BLT	PTEGON
	CMPD	#ERIGHT-3
	BGT	PTEGON
	LDA	#AOFFL1-2	ASSUME NEAR TRACKING LINE 1
	LDB	PPOSY+1,U
	CMPB	#AOFFL1+AOFFUD
	BLO	L010_015		 BR=TRACK ON LINE 1
	LDA	#AOFFL2-2	ASSUME NEAR TRACKING LINE 2
	CMPB	#AOFFL2+AOFFUD
	BLO	L010_015		 BR=TRACK ON LINE 2
	LDA	#AOFFL3-2	ASSUME NEAR TRACKING LINE 3
L010_015:	SUBA	PPOSY+1,U	ABOVE OR BELOW LINE?
	BEQ	L030_009
	BMI	L020_013		BR=ABOVE THE LINE
	LDD	PVELY,U
	CMPD	#$0100
	BGE	L011_002
	ADDD	#$0020
	STD	PVELY,U
L011_002:	JMP	PTEDIR
*
L020_013:	LDD	PVELY,U
	CMPD	#-$0100
	BLE	L021_001
	SUBD	#$0020
	STD	PVELY,U
L021_001:	JMP	PTEDIR
*
L030_009:	LDD	PVELY,U
	ASRA
	RORB
	ASRA
	RORB
	STD	PVELY,U
	JMP	PTEDIR
*
PTEGON	JSR	CPLYR		ERASE PLAYER FOR THE LAST TIME
	LDA	#$7F		NO LONGER COLIDE WITH PTERODACTYL
	ANDA	PID,U
	STA	PID,U
	PCNAP	60
	JMP	VSUCIDE		end the process
*
*	THE INTELLIGENCE OF A KILLED PTERODACTYL
*
PTEKLP	LDB	#FLY2-FLY1
	STB	PIMAGE,U
	JSR	SRCADP
	PCNAP	4
	BRA	PTEKL2
*
PTEKLL	COM	PFACE,U
	LDA	PCHASE,U	KILLED A BAITER?
	BEQ	PTEKL2
	DEC	NBAIT
	JSR	PATC10
********	CLR	PCHASE,U
PTEKL2	JSR	CPLYR		ERASE THE PTERODACTYL
	LDB	#FLY3-FLY1
	STB	PIMAGE,U
	JSR	SRCADP
	PCNAP	4
	JSR	CPLYR		ERASE THE PTERODACTYL
	LDA	PJOYT,U
	ANDA	#$FC				;#!N$03		FACE RIGHT 1/2 WAY THROUGH THE DEATH DANCE
	BNE	L010_016
	CLR	PFACE,U
L010_016:	DEC	PJOYT,U		TIME OUT?
	BPL	PTEKLP
*
	LDA	#3		3 FRAME ANIMATION
	STA	PFRAME+2,U
	LDD	ASH1R_SHRAM		PTERODACTYL ASHES TO ASHES
	STD	PFRAME+3,U
L020_014:	LDY	PFRAME+3,U
	BSR	PTEASH
	PCNAP	2
	LDY	PFRAME+3,U
	BSR	PTEASH
	STY	PFRAME+3,U	we must maintain our why
	PCNAP	6		while we sleep a little
	DEC	PFRAME+2,U
	BNE	L020_014
	JSR	CPLYR		ERASE THE PTERODACTYL
	LDD	#$1200		 (THE ENTIRE BOX)
	STD	WCDMA,X
*
	PCNAP	2		WAIT FOR ERASED PTERODACTYL
	LDD	#MSGTH1*256+($11*PTC)	PTERODACTYLE SCORE & COLOR
	STD	PRDIR,U
	JMP	SCRAIR
*
*
PTEASH	LDD	PPOSX,U		GET DESTINATION OF ANIMATION
	RORA
	RORB
	TFR	B,A
	LDB	PPOSY+1,U
	SUBB	#11-1		GO FROM BOTTOM OF PTERO TO TOP OF IT
**PTE	SUBB	#13-1	11-1		GO FROM BOTTOM OF PTERO TO TOP OF IT
	STD	PFRAME,U	Adjust destination for centering on cliffs
	JMP	CLIFER
*
*	PTERODACTYL (FOR A WAVE)
*
PTERST	CLR	PCHASE,U	NOT A BAITER
*
*	BAITER PTERODACTYL (USER MUST PRESET PCHASE,U TO NON-ZERO)
*
BAITST	LDA	PCHASE,U	SAVE THIS REGISTER
	LDB	#PPOSX		CLEAR OUT PTERODACTYLS WORKSPACE
L010_017:	CLR	B,U
	INCB
	CMPB	#PNBR
	BLO	L010_017
	STA	PCHASE,U
********CLR	PFACE,U		ASSUME BUZZARD COMING IN FROM LEFT HAND SIDE
	LDA	#2
	STA	PVELX,U		AT MAXIMUM WARP SPEED
	LDX	#ELEFT+1
	JSR	VRAND
	BCC	L001_007
	COM	PFACE,U
	NEG	PVELX,U		AT REVERSED SPEED
	LDX	#ERIGHT-1
L001_007:	STX	PPOSX,U
	LDD	#0
	STD	AREA1		AREA2, FIND A CLEAR AREA TO APPEAR FROM
	CLR	AREA3
********STD	PVELY,U		INITIAL Y VELOCITY
********STD	PSTATE,U	FLYING STATE
********STD	PPICH,U
********STD	PPICR,U
	LDX	TARPLY		NEAR PLAYER1?
	BEQ	L002_005		 BR=NO PLAYER1
	JSR	SELARE
	LDX	TARPL2
	BEQ	L002_005		 BR=NO PLAYER2
	JSR	SELARE
L002_005:	LDX	#$D3-2		FOR BOTTOM CLIF5 AREA
	LDA	AREA3
	BEQ	L003_004
	LDX	#$8A-10		FOR MIDDLE CLIF3 AREA
	LDA	AREA2
	BEQ	L003_004
	LDX	#$51-20		FOR TOP CLIF1 AREA
L003_004:	STX	PPOSY,U
	LDD	#PTERO		INTELLIGENCE
	STD	PJOY,U
	LDX	#SNPTEI		PTERODACTYL INTRODUCTION CALL
	JSR	VSND
	LDA	#$80		ENABLE COLISIONS
	ORA	PID,U
	STA	PID,U
	LDA	#$80		A VERY HIGH LANTZ
	STA	PLANTZ,U	 (TO KILL PLAYER)
********CLR	PBUMPY,U	RESET BUMP REGISTERS
********CLR	PBUMPX,U
	LDD	#P7DEC		THE 7TH INTELLIGENT PERSON IN THE GAME
	STD	PDECSN,U
********CLR	PACCX,U		CHANGE ANIMATION FRAME NOW!
********CLR	PIMAGE,U
********CLR	PFRAME,U
	LDA	#120		2 SEC DELAY BETWEEN ATTACKS
	STA	PFEET,U
	LSRA			1 SECOND WAIT TILL FIRST SEEK
	JSR	PATCH8
********	STA	PPVELX,U
	CLR	PTIMUP,U	NEEDS TO BE DONE
	BRA	PTESTF
*
*	PTERODACTYL FLYING METHODS
*
PTEFLY	PCNAP	1
	JSR	AIROVR		FLYING OVERHEAD
PTESTF	DEC	PACCX,U		DECREMENT FRAME HOLDING TIME
	BGT	L030_010		 BR=STILL THIS FRAME
	LDB	PFRAME,U	NEXT FRAME
	SUBB	#2
	BPL	L020_015		 BR=DO NOT RESET SEQUENCE
	LDB	#(4-1)*2	RESET SEQUENCE
L020_015:	STB	PFRAME,U
	LDX	#PTESEQ		GET MORE DATA FROM SEQUENCE TABLE OF FRAMES
	LDD	B,X
	STA	PIMAGE,U	NEW IMAGE TO SHOW
	STB	PACCX,U		TIME TILL NEXT FRAME CHANGE
L030_010:
*
	LDX	#FLYXP		ADD IN BUMPX,Y; AND VELX,Y; NO GRAVITY!
	LDD	PVELY,U
	JSR	ADDGRX		ADD IN GRAVITY
	BSR	SRCADP
	BRA	PTEFLY
*
*	SOURCE ADDRESS FOR IMAGE CALCULATION
*	 AND WRITE THE BIRD
*	 AND CHECK IF HIT BACKGROUND
*	 PIMAGE,U HAS FRAME OFFSET
*
SRCADP
	LDB	PIMAGE,U	GET IMAGE NBR
	LDA	PFACE,U		FACE OFFSET
	BPL	L001_008
	ADDB	#6
L001_008:	LDX	IPTERO_SHRAM		CALCULATE PTERODACTYL FRAME OFFSET
	ABX
	STX	PPICH,U
	LDB	PPOSY+1,U	COLISION DETECT DATA
	CMPB	#$D3-1		MINIMUM POSITION IN THE Y DIRECTION
	BLO	L012_001
	CLR	PVELY,U		STOP FALLING
	CLR	PVELY+1,U
	LDB	#$D3-1
	STB	PPOSY+1,U
L012_001:	STB	PCOLY1,U	MAINTAIN BOTTOM LINE OF COLISION BOX
**PTE	SUBB	#11-1	 	MAINTAIN TOP LINE OF COLISION BOX
	SUBB	#11-1	 	MAINTAIN TOP LINE OF COLISION BOX
	STB	PCOLY2,U
	LDD	PPOSX,U
	ADDD	#28-1
	STD	PCOLX,U		MAINTAIN COLISION DETECT POINTERS
	JSR	WNORIA		WRITE THE BIRD ONLY!
	LDA	NRIDER		WAVE OVER?
	BEQ	L048_001		 BR=YES
	LDA	DBAIT		BAITERS SHOULD REMOVE THEMSELVES?
	BEQ	L049_000		 BR=NO
	LDA	PCHASE,U	IS THIS A BAITER?
	BEQ	L049_000		 BR=NO
	CLR	PCHASE,U	NO LONGER A BAITER
	DEC	NBAIT		BUT, KEEP TABS ON 1 LESS BAITER
L048_001:	LDD	PJOY,U		ALREADY IN AUTO OFF ROUTINE?
	SUBD	#PTEOFF
	BHS	L049_000		 BR=YES
	LDD	#PTEOFF		NO, END ALL PTERODACTYLS
	STD	PJOY,U
L049_000:
*
*	CHECK IF PTERODACTYL HIT BACKGROUND
*
CKPTER	LDX	PPOSX,U
	LDY	PPOSY,U
	LDA	BCKXTB,X
**PTE	ORA	BCKXTB+6,X
	ORA	BCKXTB+28-18,X
	ANDA	BCKYTB+6,Y
**PTE	ANDA	BCKYTB+7,Y	CLIFF COLISION AREA
********	TFR	A,B
********	ORB	BCKRFS		REFRESH CLIFF AREA
********	STB	BCKRFS
	ANDA	BCKYTB+7,Y
	BEQ	L001_009
	JMP	BCKCOL		COLIDE WITH BACKGROUND
L001_009:	RTS
*
*	PTERODACTYLS FLYING ANIMATION SEQUENCE
*
PTESEQ	FCB	FLY1-FLY1,8
	FCB	FLY2-FLY1,8
	FCB	FLY3-FLY1,8
	FCB	FLY2-FLY1,8
*
*	PTERODACTYLS X VELOCITY TABLE
*
	FDB	-$0300
	FDB	-$0180
	FDB	-$00C0
	FDB	-$0060
FLYXP	FDB	$0000
	FDB	$0060
	FDB	$00C0
	FDB	$0180
	FDB	$0300
	FCB	$5F	JZAPPER FUDGE
*
*	LAVA TROLL PART 1 - THIS IMAGE WILL BE BEHIND THE BIRD
*			THE HAND WILL COME UP FOLOWING THE PLAYER
*			THEN TRACK THE PLAYER IN THE Y DIRECTION,
*			UNTIL EITHER THE HAND MISSES OR GETS ITS TARGET.
*
LAVATC	JSR	CLAVAT		ERASE LAVA TROLL
LAVAT1	JSR	LAVVFY		MAKE SURE TARGET STILL EXISTS
	STD	PPOSX,U		GOT HIS X POSITION
	LDA	PFRAME,U	GRAB FRAME ?
	DEC	PJOYT,U		TIME FOR NEXT FRAME?
	BGT	LT1TRK		 BR=NO
	LDA	LAVTIM		RESET TIMER
	STA	PJOYT,U
	LDA	PFRAME,U	NEXT FRAME
	CMPA	#5*6		CURRENT FRAME HAND EXTENTED FRAME?
	BEQ	LT1HT		 BR=YES
	ADDA	#6
	STA	PFRAME,U
LT1TRK	JSR	WLAVAT		WRITE THE LAVA TROLL
	NAPGO	1,LAVATC	WAKE-UP AT CLEAR LOOP
*
LT1HT	LDB	PPOSY+1,Y
	ADDB	#10-7		OFFSET FOR PROPER HAND GRIP
	CMPB	PPOSY+1,U
	BEQ	LT1GRP		GO GRAB THE PLAYER
	BHI	L001_011		BR=HAND TOO HIGH GO DOWN
	DEC	PPOSY+1,U	RAISE THE HAND
	BRA	L002_006
*
L001_011:	INC	PPOSY+1,U
L002_006:	JSR	WLAVAT
	PCNAP	1
	JSR	CLAVAT
	JSR	LAVVFY		MAKE SURE TARGET STILL EXISTS
	STD	PPOSX,U		GOT HIS X POSITION
	BRA	LT1HT		LOOP UNTIL THE CORRECT HEIGHT
*
*	LAVA TROLL CAN GRIP THE PLAYER
*
LT1GRP	LDX	#SNTROL		MAKE THE SOUND OF THE LAVA TROLL
	JSR	VSND		 GRIPPING THE PLAYER
	LDD	PID,U		(& PPRI,U)CREATE PROCESS TO GRIP THE BIRD
	INCA			LAVA TROLL GRAP I.D. = LAVA TROLL +1
	LDU	PLINK,U		GET VICTIMS WORKSPACE
	LDX	#ADDLAV		THE VICTIMS GRAVITY IS NOW BASED UPON
	STX	PADGRA,U	 THE LAVA TROLLS GRAVITY
	LDX	#LAVAT2
	JSR	VCUPROC		HOLD THE BIRD IS AFTER THE VICTEM WORKSPACE
	LDU	PEXEC		GET WORKSPACE BACK
	LDD	PPOSX,U		TRANSFER POSITION TO NEW PROCESS
	STD	PPOSX,Y
	LDD	PPOSY,U
	STD	PPOSY,Y
	LDD	PJOY,U		GIVE THE VICTIM'S PROCESS ADDRESS
	STD	PJOY,Y
	STU	PDIST,Y		GIVE LAVAT2 LAVAT1'S PROCESS ADDRESS
	LDD	#0
	JSR	PATCH1		APPLY A LAVA TROLL PATCH
*******	STD	PPICH,Y		RESET PICTURE AREA IN CASE OF PREMATURE CLEAR
L001_012:	PCNAP	2
	JSR	LAVVFY		MAKE SURE TARGET STILL EXISTS
	BRA	L001_012		LOOP UNTIL SOMETHING HAPPENS
*
************************************
*
*	LAVA TROLL PULLING DOWN PLAYER (HANDLED IN PLAYER)
*	 ACTUALLY THE LAVA TROLL JUST FOLLOWS THE PLAYER DOWN
*
LAV2LP	JSR	CLAVAT
LAVAT2	LDY	PPREV		NOW THE VICTIM IS BEFORE THIS ROUTINE
	JSR	LAVVI2		MAKE SURE TARGET STILL EXISTS
	BNE	LT2SUC		 BR=OUT OF ITS HANDS
	LDX	PDIST,U		MAINTAIN POSITION IN LAVAT1
	STD	PPOSX,U		GOT HIS X POSITION
	STD	PPOSX,X
	LDD	PPOSY,Y
	ADDB	#-7+17		CORRECT HAND HOLD
	STD	PPOSY,U
	STD	PPOSY,X
	LDA	#6*6		FINAL HOLDING FRAME
	STA	PFRAME,U
	JSR	WLAVAT
	LDX	#LAV2LP
	LDA	#1		NAP 1
	JMP	PATCH2
********	JMP	VNAPTIM
*
LT2DIE	JSR	CLAVAT
LT2SUC	JMP	VSUCIDE		end the process
*
*	VICTIM WITHIN RANGE DETERMINATION.
*	 REG.D = TRACKING PPOSX
*	 STATUS - RETURNS IF IN RANGE
*	 STATUS - JMPS TO LAVATF ROUTINE IF OUT OF RANGE
*
LAVVFY	BSR	LAVVIC		CHECK IF VICTIM IS STILL WITHIN RANGE
	BNE	LAVATF		 BR=NO
	RTS
*
*	VICTIM WITHIN RANGE DETERMINATION.
*	 REG.D = TRACKING PPOSX
*	 STATUS .EQ. STILL WITHIN RANGE
*	 STATUS .NE. OUT OF RANGE
*
LAVVIC	LDY	PLINK,U		IS PROCESS STILL THERE?
LAVVI2	CMPY	PJOY,U
	BNE	LVVOUT		 BR=NO
LAVVI3	LDA	PID,Y		IS THE PROCESSES IMAGE STILL ALIVE
	BPL	LVVOUT		 BR=NO
	LDA	PSTATE,Y	THIS OBJECT IS STILL IN THE AIR?
	BNE	LVVOUT		 BR=NO
	LDA	PPOSY+1,Y	IS IMAGE TOO HIGH?
	CMPA	#FLOOR+7-32
	BLO	LVVOUT		 BR=YES
	LDD	PPOSX,Y		OUT OF REACH LEFT SIDE
	ADDD	#-2		FUDGE FOR EXACT X CO-ORDINATE MATCH
	CMPD	#54-14
	BLE	LVVIN
	CMPD	#240		(CLIF5 BOUNDS)
	BLT	LVVOUT
LVVIN	ORCC	#$04		.EQ. IN RANGE
	RTS
*
LVVOUT	ANDCC	#$FB			;#!N$04		.NE. OUT OF RANGE (FOR ONE REASON OR ANOTHER)
	RTS
*
*	JZAP - I think this is more copy protection although I'm not sure what incrementing LXPOS2+1 will do (mybe kill game when lava is drawn)
*
JZAP:
	IF CopyProtect
	LDX	JZAPST
	ELSE
	RTS
	NOP
	NOP			; RTS and NOP,NOP keeps data in the same location, for testing
	ENDIF

	CLRA
L001_013:	ADDA	,-X
	CMPX	#$D000
	BNE	L001_013
	EORA	#$A1		JZAPPER DATA
	BEQ	L002_007
	INC	LXPOS2+1
L002_007:	RTS
*
*
*	LAVA TROLL RELEASING HIS GRIP
*		DROP HAND
*
LAVATF	LDA	PFRAME,U	NEXT FRAME
	ADDA	#-6
	STA	PFRAME,U
	BLE	LT1DON		 BR=OUT OF FRAMES
LT1DRP	JSR	WLAVAT		WRITE THE LAVA TROLL
	LDA	#5
	LDX	#LAVATC		WAKE-UP AT BEGINNINNG
	JMP	VNAPTIM
*
LT1DON	DEC	LAVNBR		FREE UP LAVA TROLL(S)
	JMP	VSUCIDE		end the process
*
*	ERASE LAST "PPICH,U" LAVA TROLL IMAGE
*
CEGG	EQU	*		CLEAR EGG ROUTINE
CLAVAT	LDY	PPICH,U		AND CLEAR LAVA TROLL ROUTINE
	BEQ	WLARTS
	CLR	PPICH,U
	CLR	PPICH+1,U
	JSR	CLREGY		ERASE THE IMAGE
	JMP	CLIPER		CLIP THE IMAGE (CLOSE TO THE BOTTOM)
WLARTS	RTS
*
*	WRITE "PFRAME,U" LAVA TROLL IMAGE
*
WLAVAT	LDA	PFRAME,U
	SUBA	#6
	BMI	WLARTS		BR=NO LEGAL FRAME TO DISPLAY
	LDY	ILAVAT_SHRAM
	LEAY	A,Y
	STY	PPICH,U
	JSR	VWR1CLS
WLADMA	JSR	WRHOR2
	JMP	CLIPER		CLIP THE IMAGE (CLOSE TO THE BOTTOM)
*
*	THORSE
*
THORSE	PCNAP	30
	LDD	#$2502		LEFT1,FLAP1,CREDIT1,RIGHT2
	BSR	L080_001
	BNE	THORSE
L010_018:	BSR	L080_001
	BEQ	L010_018
	PCNAP	15		1/4 SECOND TILL NEXT SWITCH CLOSURES
	LDD	#$1204		RIGHT1,CREDIT2,FLAP2
	BSR	L080_001
	BNE	THORSE
L015_001:	BSR	L080_001
	BEQ	L015_001
	PCNAP	15
	LDD	#$0506		LEFT1,FLAP1,RIGHT2,FLAP2
	BSR	L080_001
	BNE	L010_018
	JSR	SCCLR          * CLEAR SCREEN
	ORCC	#$FF
	LDD	#$00FF
	STD	$C000
	LDU	#TROMSG
	LDX	#$1030
	PSHS	X
	LDB	#$11
L030_011:	LDA	,U+
	EORA	#$9B
	SUBA	#$35
	BMI	L050_001
	BEQ	L040_003
	JSR	OUTCHR
	BRA	L030_011
*
L040_003:	LDX	,S
	LEAX	$10,X
	STX	,S
	BRA	L030_011
*
L050_001:	LDX	#14285/2		1/2 SEC DEBOUNCE
L055_000:	LDA	#WDATA
	STA	WDOG
	BSR	READEM   * Read player controls Player 1 bits are in A and Player 2 bits is in B
	SUBD	#$0506 * Check for P1 Flap, P1 Jeft  & P2 Flap, P2 Right
	BEQ	L050_001
	LEAX	-1,X
	BNE	L055_000
	JMP	[$FFFE]
*
L080_001:	STD	PRAM,U
	LDD	,S++
	STD	PRAM+2,U
	PCNAP	1
	BSR	READEM   * Read player controls Player 1 bits are in A and Player 2 bits is in B
	TFR	D,X
	LDD	PRAM,U
	CMPX	PRAM,U
	JMP	[PRAM+2,U]
*
READEM:
;	LDA	#$08        * %00001000
;	ORA	WCPIAB
;	STA	WCPIAB
;	LDA	WPIAA_P1

  LDD   WPIAA_P1  * Loads A with WPIAA_P1 and B with WPIAA_P2
	ANDA	#$37      * %00110111 = check only for P1 Start,P2 Start,Flap,Right,Left
;	LDB	#$F7			;#!N$08         * %11110111
;	ANDB	WCPIAB
;	STB	WCPIAB
;	LDB	WPIAA_P2
	ANDB	#$07      * %00000111 = check for P2 Flap,Right,Left
  RTS

*
JZAPST	FDB	$9000
*
*
*	Centrial Intelligence Agency
*	INITILIZATION
*
*
CIA:
	CLR	WAVBCD		WAVE STARTS AT ZERO
	LDD	#WAVRTS
	STD	PWPREV,U	NO PREVIOUS WAVE TO COMPLETE
	LDD	#WAVTBL-WLEN
	STD	PWAVE,U
	LDA	#BAISND-BAISBL-1	RESET BAITER TIME TABLE
	STA	PBAITS,U

  IF DestroyBridge
	JSR	STBRID		START BRIDGE COLAPSING ROUTINE, LEFT TO RT.
	LDD	#-$20		START POSITION IN X DIRECTION
	STD	PDIST,Y
	LDA	#1		OFFSET FOR FLAME FROM LANDING TABLE
	STA	PVELX,Y
	LDA	#1		DIRECTION TO TRAVEL
	STA	PJOYT,Y
	JSR	STBRID		START BRIDGE COLAPSING ROUTINE RT. TO LEFT
	LDD	#$13F		START POSITION IN X DIRECTION
	STD	PDIST,Y
	LDA	#7		OFFSET FOR FLAME FROM LANDING TABLE
	STA	PVELX,Y
	LDA	#-1		DIRECTION TO TRAVEL
	STA	PJOYT,Y
  ENDIF

	PCNAP	32
	BRA	IWAVE2
*
*	INTER WAVE SET-UP & MESSAGES MAIN LOOP
*
IWAVE	LDA	TRSMALL		SMALLER TRANSPORTER AREA?
	BEQ	L001_014		 BR=NO
	DEC	TRSMALL
L001_014:	LDX	#GA1		GET GAME ADJUST
	JSR	RCMSA
	STA	,-S		EXTRA RAM FOR ADJUST OFFSET
	LDX	#DYTBL		ROM INITILIZATION
	LDY	#DYNADJ		RAM AREA
L002_008:	LDB	2,Y		TIME TO CHANGE THE VALUE?
	BEQ	L004_002		 BR=YES
	DEC	2,Y
	BNE	L015_002		 BR=NO
L004_002:	LDB	,S		DIFFICULTY LEVEL
	ADDB	#DYWTIM*2	GET OFFSET FOR TIME TABLE
	ASRB			GET BYTE OFFSET
	LDB	B,X		GET TIME BYTE
	BCS	L005_002		 BR=LOWER NIBBLE
	LSRB
	LSRB
	LSRB
	LSRB
L005_002:	ANDB	#$0F
	STB	2,Y		NEW TIME
	LDB	DYWINC,X
	SEX
	BMI	L006_000
	ADDD	,Y		NEW VARIABLE
	CMPD	DYWEND,X
	BLT	L008_000
	BRA	L007_000
*
L006_000:	ADDD	,Y		NEW VARIABLE
	CMPD	DYWEND,X
	BGT	L008_000
L007_000:	LDD	DYWEND,X
L008_000:	STD	,Y
L015_002:	LEAX	DYWLEN,X
	LEAY	3,Y
	CMPX	#DYEND
	BLO	L002_008
	PULS	A		RESTORE STACK
*
	LDA	SAFRAM		RASIE LAVA IF POSSIBLE
	CMPA	#$E0
	BLS	IWAVE2		 BR=AT TOP LEVEL
	SUBA	#$5
	STA	SAFRAM
IWAVE2:
	LDA	TBRIDG     ;TBRIDGE		TIME TILL BURNS YOUR BRIDGES BEHIND YOU
	BEQ	WTROLL
	DEC	TBRIDG         ;TBRIDGE
	BNE	WNRM
	JSR	STBRID		START BRIDGE COLAPSING ROUTINE, LEFT TO RT.
	LDD	#-$20		START POSITION IN X DIRECTION
	STD	PDIST,Y
	LDA	#1		OFFSET FOR FLAME FROM LANDING TABLE
	STA	PVELX,Y
	LDA	#1		DIRECTION TO TRAVEL
	STA	PJOYT,Y
	JSR	STBRID		START BRIDGE COLAPSING ROUTINE RT. TO LEFT
	LDD	#$13F		START POSITION IN X DIRECTION
	STD	PDIST,Y
	LDA	#7		OFFSET FOR FLAME FROM LANDING TABLE
	STA	PVELX,Y
	LDA	#-1		DIRECTION TO TRAVEL
	STA	PJOYT,Y
	BRA	WNRM
*
WTROLL	LDA	TTROLL		TIME TILL LAVE TROLL STARTS
	BEQ	WNRM
	DEC	TTROLL
	BNE	WNRM		 BR=LAVA TROLL NOT AWAKE YET
	 LDX	#LAVAF		LAVA FLAMES
	 LDD	#$69FF
	 LDU	PDUMMY		ENTER AS 2ND PROCESS IN TOTAL PROCESS LIST
	 JSR	VCUPROC
	LDD	#FLOOR+16	START FLAME UNDER LAVA
	STD	PPOSY,Y		 LEFT & RIGHT FLAME Y POSITION
	LDD	#ELEFT+30	 LEFT FLAME X POSITION
	STD	PPOSX,Y
	CLR	PFRAME,Y
	 LDX	#LAVAF		LAVA FLAMES
	 LDD	#$69FF
	 JSR	VCUPROC
	 LDU	PEXEC
	LDD	#FLOOR+16	START FLAME UNDER LAVA
	STD	PPOSY,Y		 LEFT & RIGHT FLAME Y POSITION
	LDD	#ERIGHT-30	 RIGHT FLAME X POSITION
	STD	PPOSX,Y
	LDA	#2
	STA	PFRAME,Y
	LDA	#4		MAKE THIS FLAME OUT OF SYNC OF THE OTHER
	STA	PNAP,Y
WNRM	CLR	EGGS1		RESET NUMBER OF EGGS KILLED BY PLAYER 1
	CLR	EGGS2		RESET NUMBER OF EGGS KILLED BY PLAYER 2
	CLR	WSMART		RESET NUMBER OF ALLOWED SMART ENEMIES
	LDA	#255
	STA	WENEMY		MAXIMIM ENEMIES FOR A NORMAL WAVE
	LDA	#1		NO ENEMY SLOW DOWN
	STA	EMYTIM
	CLR	PMSG0,U		RESET WAVE N MESSAGE SENT AREA
	CLR	PMSG1,U		RESET MESSAGE BUFFERS
	CLR	PMSG2,U
	CLR	PMSG3,U
	CLR	PMSG4,U
	CLR	PMSG5,U
  LDA	WAVBCD		BCD WAVE NUMBER
  ADDA	#$01		 NEXT WAVE NUMBER 0-99
	DAA
	STA	WAVBCD
*
*	CLEAN UP THE END OF A WAVE
*
	JSR	[PWPREV,U]	CALL END OF WAVE ROUTINE
	JSR	PATC11		CLEAN UP GLADIATOR WAVE
********	JSR	EMSGS		ERASE ANY END OF WAVE MESSAGES
	PCNAP	1
	JSR	WAVEN		PUT UP WAVE NUMBER
	LDD	#WAVRTS
	STD	PWPREV,U	NO PREVIOUS WAVE TO COMPLETE
	LDX	PWAVE,U		GET WAVE TABLE ADDRESS
	LDA	WSTATUS,X	GET OLD CLIFF STATE
	LEAX	WLEN,X
	CMPX	#WTBEND		AT END OF TABLE??
	BLO	L001_015
	LDX	#WTBRST		YES, RESTART AT NEW POINT
L001_015:	STX	PWAVE,U		NEW TABLE ADDRESS
	EORA	WSTATUS,X	ANY CLIFFS CHANGED?
	ANDA	#WBCLS		ALL CLIFFS; WBCL1+WBCL2+WBCL4
	BEQ	L004_003
	LDB	WSTATUS,X	GET CORRECT STATE OF CLIFF
	LDX	#WCLFTB		CLIFF INFORMATION
	JSR	WCLFEW		CLIFF1L ERASE/WRITE ROUTINE
	LDX	#WCLFT2
	JSR	WCLFEW		CLIFF1R ERASE/WRITE ROUTINE
	LDX	#WCLFT3
	JSR	WCLFEW		CLIFF2 ERASE/WRITE ROUTINE
	LDX	#WCLFT4
	JSR	WCLFEW		CLIFF4 ERASE/WRITE ROUTINE
L004_003:	LDX	PWAVE,U		GET WAVE AREA TO WORK FROM
	LDA	WSTATUS,X	EARLY PERSUE?
	BITA	#WBPER
	BEQ	L044_000		 BR=NO
	LDA	WPERSUE,X	SET PERSUE VARIABLE
	ANDA	#$0F
	STA	WSMART
L044_000:	LDB	WSTATUS,X		CALL WAVE STARTER ROUTINE
	ANDB	#WBJSR0+WBJSR1+WBJSR2	MASK OFF OTHER BITS
	LDX	#WJSRTB		JSR TABLE
	JSR	[B,X]
WPAUSE	LDA	#180/6		WAIT A SECOND
WPAUS2	JSR	WAVDEL
	JSR	WATWAV		WAIT TILL END OF WAVE
WPAUS3	JSR	EMSGS		ERASE ALL MESSAGES
	LDA	[PWAVE,U]	INTRODUCE BOUNDARS
	LSRA
	LSRA
	LSRA
	LSRA
	BEQ	L002_009		BR=NO BOUNDARS
	LDX	#P4DEC		(INTELLIGENCE)
	JSR	WCREATE		CREATE THEM
L002_009:	LDA	[PWAVE,U]	INTRODUCE BOUNDARS
	ANDA	#$0F
	BEQ	L003_005		BR=NO BOUNDARS
	LDX	#P5DEC		(INTELLIGENCE)
	JSR	WCREATE		CREATE THEM
L003_005:	LDX	PWAVE,U
	LDA	WLORD,X		INTRODUCE SHADOW LORDS
	LSRA
	LSRA
	LSRA
	LSRA
	BEQ	L004_004		BR=NO BOUNDARS
	LDX	#P6DEC		(INTELLIGENCE)
	JSR	WCREATE		CREATE THEM
L004_004:
WBEGIN	LDA	GOVER		IS THE GAME OVER?
	BEQ	L005_003		 BR=YES
	LDX	PWAVE,U
	LDA	WPERSUE,X	SET PERSUE VARIABLE
	ANDA	#$0F
	STA	WSMART
L005_003:	LDA	PBAITS,U	GET BAITERS TABLE DRIVEN TIME
	BEQ	L006_001
	DEC	PBAITS,U
L006_001:	LDX	#BAISBL
	LDA	A,X		GET TIME TABLE START POSITION
	STA	PBAITN,U
	INCA
	ASLA
	LDX	#BAITBL
	LDD	A,X		BAITER INITIAL SLEEP TIME
	STD	CBAIT
	CLR	NBAIT		NUMBER OF BAITERS ON THE SCREEN
	CLR	DBAIT		NO BAITER DELAY
*
*	GAME PLAY - DETECT END OF WAVE, BAITER SENDOFF, INTELLIGENCE INCREMENT
*
EMY2	LDA	#112		AFTER 15 SECONDS BUMP NBR OF INTELLIGENT PEOPLE
	STA	PDELAY,U
EMYOK	PCNAP	8
	LEAX	,U
	LDA	NRIDER		ALL ENEMIES DEAD?
	LBEQ	IWAVE		 BR=YES, END OF WAVE
	LDA	DBAIT		DELAY BAITERS?
	BEQ	L009_001		 BR=NO
	DEC	DBAIT		TIME TO DECREMENT BAITER CLOCK?
	BNE	L011_003		 BR=NO
L009_001:	LDD	CBAIT
	SUBD	#1
	BGT	L010_019
	LDA	NBAIT
	CMPA	#3-1		ONLY ALLOW 3 BAITERS ON THE SCREEN
	BHI	L011_003		 BR=3 BAITERS ON SCREEN, BUT READY
	SECCR	BAITST,PTEID
	INC	NBAIT		1 MORE BAITER ON THE SCREEN
	LDA	#-1
	STA	PCHASE,Y	BAITER TYPE PTERODACTYL'S
	LDA	PBAITN,U
	BEQ	L100_000
	DEC	PBAITN,U
L100_000:	ASLA
	LDX	#BAITBL
	LDD	A,X		BAITER INITIAL SLEEP TIME
L010_019:	STD	CBAIT
L011_003:	LDA	NENEMY		NBR OF ACTIVE ENEMIES >0 ???
	BEQ	EMYOK		BR=NO ACTIVE ENEMIES, SO NO DECREMENT
	DEC	PDELAY,U	SOMEONE IS ALIVE
	BNE	EMYOK
	LDA	GOVER		IS THE GAME OVER?
	BEQ	EMY2		 BR=YES
	INC	WSMART
	BNE	EMY2
	DEC	WSMART
	BRA	EMY2
*
*	BAITER SEND OFF TIME TABLE
*
BAITBL
********	FDB	1*60/8		MAKE THE BAITER MORE FIERCE
********	FDB	3*60/8
********	FDB	5*60/8
********	FDB	7*60/8
********	FDB	15*60/8
********	FDB	15*60/8
********	FDB	15*60/8
********	FDB	15*60/8
********	FDB	15*60/8
********	FDB	15*60/8
********	FDB	15*60/8
********BWAVN	FDB	30*60/8
********BWAV2	FDB	45*60/8
********BWAV1	FDB	60*60/8
*
	FDB	1*60/8			A FASTER SEND OFF OF BAITERS (V4)
	FDB	1*60/8			THIS IS PATCH7
	FDB	1*60/8
	FDB	1*60/8
	FDB	1*60/8
	FDB	1*60/8
	FDB	3*60/8
	FDB	5*60/8
	FDB	7*60/8
	FDB	15*60/8
	FDB	15*60/8
BWAVN	FDB	30*60/8
BWAV2	FDB	45*60/8
BWAV1	FDB	60*60/8
*
*	BAITER START TABLE OFFSET
*
BAISBL	FCB	(BWAVN-BAITBL-2)/2	START OFFSET FOR WAVE N
	FCB	(BWAV2-BAITBL-2)/2	START OFFSET FOR WAVE 2
	FCB	(BWAV1-BAITBL-2)/2	START OFFSET FOR WAVE 1
BAISND	EQU	*
*
*	START BRIDGE COLAPSING ROUTINE
*
STBRID	LDU	PPREV		USE PREVIOUSES PROCESSES WORKSPACE
	LDX	#LAVAB
	LDD	#$6AFF
	JSR	VCUPROC
	LDU	PEXEC		GET OLD WORKSPACE BACK
	LDD	#FLOOR		GIVE IT SOME INITIAL CO-ORDINATES
	STD	PPOSY,Y
	CLR	PFRAME,Y	START AT FRAME 0
	RTS
*
*	CREATE ENEMIES, INPUT STACK, RETURN ADDRESS
*				REG.X INTELLIGENCE
*				REG.A NUMBER OF PEOPLE
WCREATE	STA	PDELAY,U
	LDD	,S++
	STD	PWRTS2,U	SAVE RETURN ADDRESS
	STX	PINTEL,U	THAT'S INTELLIGENCE
L010_020:	PCNAP	61
	SECCR	CREEM,EMYID
	INC	NENEMY		1 MORE ENEMY ON THE SCREEN
	INC	NRIDER		1 MORE RIDER ON THE SCREEN
	LDD	PINTEL,U
	STD	PDECSN,Y
	LDD	BUZARD_SHRAM		START HORSE
	STD	PHORSE,Y
	LDA	#4		MAXIMUM NUMBER OF EGGS TO LAY
	STA	PEGG,Y
	CLR	PCHASE,Y	NOT CHASING THE PLAYER
	LDA	TBRIDG      ;TBRIDGE		1ST, OR 2ND WAVE?
	BEQ	L020_016		 BR=NO
	LDA	#2
	STA	EMYTIM		SLOW DOWN THE ENEMY ONLY!
L020_016:	DEC	PDELAY,U
	BNE	L010_020
	JMP	[PWRTS2,U]
*
*	WAIT TILL TRUE END OF WAVE (OR GAME OVER)
*
WATWAV	LDX	,S++
	STX	PWRTS,U		RETURN ADDRESS
L001_016:	BSR	REWMSG
	LDA	GOVER		GAME OVER?
	BEQ	L002_010		 BR=YES, END OF WAVE
	LDX	PLINK,U
	BNE	L001_016
L002_010:	JMP	[PWRTS,U]
*
*	WAVE DELAY BY REG.A AMOUNT
*
WAVDEL	LDX	,S++
	STX	PWRTS,U		RETURN ADDRESS
	STA	PDELAY,U
L001_017:	BSR	REWMSG
	DEC	PDELAY,U
	BNE	L001_017
	JMP	[PWRTS,U]
*
*	RE-WRITE ALL MESSAGES
*
REWMSG	PULS	D		SAVE RETURN ADDRESS
	STD	PWRTS2,U
	LDA	GOVER		GAME OVER?
	BEQ	L001_018
	LDA	#PMSG1		REMEMBER TO START AT MESSAGE 1
	STA	PMSGA,U
	LDA	PMSG0,U		THIS MESSAGE O.K. TO PUT OUT?
	BEQ	L002_011		 BR=NO
	JSR	WAVEN
L002_011:	PCNAP	1
	LDA	PMSGA,U		GET CURRENT MESSAGE TO WRITE
	CMPA	#PMSG5		LAST MESSAGE WAS THE LAST ONE?
	BHI	L001_018		 BR=YES, RETURN TO CALLER
	LEAY	A,U
	ADDA	#PMSG2-PMSG1	NEXT MESSAGE TO WRITE
	STA	PMSGA,U
	BSR	WMSG2
	BRA	L002_011
*
L001_018:	JMP	[PWRTS2,U]
*
*	INITIAL WRITE & SAVE MESSAGE NBR & SCREEN LOCATION
*
WAVMSG	LEAY	PMSG1-4,U
L001_019:	LEAY	4,Y
	TST	,Y
	BNE	L001_019
	LDA	GOVER
	BEQ	WVMRTS
	LDD	,X
	STD	,Y
	LDD	2,X
	STD	2,Y
WMSG2	LDA	,Y
	BEQ	WVMRTS
	LDB	1,Y
	LDX	2,Y
	JMP	OUTPHR
WVMRTS	RTS
*
*	ERASE ALL MESSAGES
*
EMSGS	LDA	PMSG0,U		WAVE MESSAGE SENT TO SCREEN?
	BEQ	L011_004		 BR=NO
	JSR	WAVEN2		ERASE WAVE NUMBER
L011_004:	LEAY	PMSG1,U		MESSAGE 1
	BSR	L001_020
	LEAY	PMSG2,U		MESSAGE 2
	BSR	L001_020
	LEAY	PMSG3,U		MESSAGE 3
	BSR	L001_020
	LEAY	PMSG4,U		MESSAGE 4
	BSR	L001_020
	LEAY	PMSG5,U		MESSAGE 5
L001_020:	LDA	,Y
	BEQ	L002_012
	CLR	,Y		FORGET ABOUT THIS MESSAGE
	CLRB
	LDX	2,Y
	JMP	OUTPHR
L002_012:	RTS
*
*	ERASE/WRITE THE CLIFF,
*	 MODIFY LANDING TABLE
*		COLISION TABLE
*	 UPDATE/DESTROY THE CLIFF
*	SAVES REG.D, REG.X
*
WCLFEW	PSHS	D,X
	BITA	,X		CLIFF1 CHANGING?
	BEQ	L001_021		 BR=NO
	BITB	,X		YES, DESTROY OR CREATE?
	BEQ	L002_013		 BR=CREATE
L011_005:	LDY	5,X		HERE TO DESTROY, DESTROY TRANSPORTER AREA
	BEQ	L031_000
	LDA	#-1
	STA	,Y
L031_000:	LDA	1,X		DESTROY LANDING AREA
	BEQ	L012_002
	COMA
	LDX	#LNDXD1		 START ADDRESS
	LDY	#LNDXD2-LNDXD1	 LENGTH
	JSR	CLRBIT
	LDX	2,S
	LDA	2,X		DO NOT REFRESH THIS CLIFF
	COMA			 (REAL TIME CHECK)
	ANDA	BCKRFS
	STA	BCKRFS
	LDA	2,X		DESTROY BUMPING (COLISION) AREA
	COMA
	LDX	#BCKXD1		 START ADDRESS
	LDY	#BCKXD2-BCKXD1	 LENGTH
	JSR	CLRBIT
*
L012_002:	LDX	#SNCLIF		CLIFF DESTROYING SOUND
	JSR	VSND
	SECCR	CLFDES,$27	START CLIFF DESTROYER ROUTINE
	LDX	2,S
	LDD	[3,X]		GET CLIFF ADDRESS
	STD	PFRAME-2,Y	PASS IT TO CLIFF DESTROYER
L001_021:	PULS	D,X,PC
*
L002_013:	LDY	5,X		HERE TO CREATE, ENABLE TRANSPORTER
	BEQ	L020_017		 (AYE, AYE SCOTTY)
	CLR	,Y
L020_017:	LDA	1,X		GET BIT TO PASS FOR CREATING THE CLIFF
	STA	,-S		A DUMB WAY TO PUSH NBRS, BUT ALLOWED
	LDX	#LNDXD1		LANDING AREA IN RAM
	LDY	#LNDXS1		LANDING AREA IN ROM
L022_000:	LDA	,S		GET CHANGING BIT
	COMA
	ANDA	,X
	STA	,X
	LDA	,S		GET VALID BITS FROM ROM
	ANDA	,Y+
	ADDA	,X		PUT IN RAM
	STA	,X+
	CMPX	#LNDXD2
	BLO	L022_000
	LDX	1+2,S
	LDA	2,X		GET BIT TO PASS FOR CREATING THE CLIFF
	STA	,S		SAVE ON STACK
	ORA	BCKRFS		REFRESH THIS CLIFF (MAKE IT APPEAR)
	STA	BCKRFS
	LDX	#BCKXD1		LANDING AREA IN RAM
	LDY	#BCKXS1		LANDING AREA IN ROM
L023_000:	LDA	,S		GET CHANGING BIT
	COMA
	ANDA	,X
	STA	,X
	LDA	,S		GET VALID BITS FROM ROM
	ANDA	,Y+
	ADDA	,X		PUT IN RAM
	STA	,X+
	CMPX	#BCKXD2
	BLO	L023_000
	LDA	,S+		REMOVE DATA
	PULS	D,X
	STD	PWREGD,U	SAVE REG.D
	LDD	,S++		SAVE RETURN ADDRESS
	STD	PWRTS,U
	PCNAP	1		FREE UP SOME TIME
	LDD	PWREGD,U	RESTORE REGISTER D
	JMP	[PWRTS,U]	RETURN
*
*	CLEAR BIT
*	 REG.X = START/CURRENT ADDRESS
*	 REG.Y = LENGTH IN BYTES
*	 REG.A = BIT PATTERN TO CLEAR
*
CLRBIT	TFR	A,B
	ANDB	,X
	STB	,X+
	LEAY	-1,Y
	BNE	CLRBIT
	RTS
*
*	PRINT THE WAVE NUMBER
*
WAVEN	LDD	#MSW06*256+WHI*$11	PUT UP 'WAVE XX'
	STA	PMSG0,U		THIS MESSAGE HAS BEEN PUT UP
	BRA	WAVEN3
WAVEN2	LDD	#MSW06*256+$00	ERASE 'WAVE XX'
	CLR	PMSG0,U		THIS MESSAGE HAS BEEN ERASED
WAVEN3	LDX	#$3A60
	JSR	OUTPHR
	LDA	WAVBCD
	BITA	#$F0
	BNE	L001_022
	ORA	#$F0
L001_022:	JMP	OUTBCD
*
*	CLIF DESTROY/CREATE DATA TABLE
*
WCLFTB	FCB	WBCL1L,$01,$03
	 FDB	 CLIF1L_SHRAM,0
WCLFT2	FCB	WBCL1R,$00,$00
	 FDB	 CLIF1R_SHRAM,0
WCLFT3	FCB	WBCL2,$02,$04
	 FDB	 CLIF2_SHRAM,CURTR1
WCLFT4	FCB	WBCL4,$10,$40
	 FDB	 CLIF4_SHRAM,0
*
INTRO1	FDB	MSW00*256+WHI*$11,$2D7D	INTRO #1
INTRO2	FDB	MSW01*256+WHI*$11,$339B	INTRO #2
COOP1	FDB	MSW02*256+WHI*$11,$367D	CO-OP (TEAM) WAVE INTRO #1
COOP2	FDB	MSW03*256+WHI*$11,$1C9B	CO-OP (TEAM) WAVE INTRO #2
COOP3	FDB	MSW04*256+WHI*$11,$1AB6	 NO BONUS AWARDED
COOP4	FDB	MSW05*256+WHI*$11,$0BB6	 BONUS AWARDED
SURV1	FDB	MSW14*256+WHI*$11,$317D	SURVIVAL WAVE INTRO #1
SURV3	FDB	MSW16*256+WHI*$11,$26B6	 NO BONUS AWARDED
SURV4	FDB	MSW15*256+PL1*$11,$08B6	 BONUS AWARDED (RED GUY)
SURV5	FDB	MSW15*256+PL2*$11,$3FB6	 BONUS AWARDED (GREEN GUY)
GLAD1	FDB	MSW08*256+WHI*$11,$307F	GLAD WAVE INTRO #1
GLAD2	FDB	MSW09*256+WHI*$11,$2E90	GLAD WAVE INTRO #2
GLAD3	FDB	MSW10*256+WHI*$11,$1C9B	GLAD WAVE INTRO #3
GLAD4	FDB	MSW11*256+WHI*$11,$32B6	 NO AWARD
GLAD5	FDB	MSW12*256+PL1*$11,$10B6	 RED - 3000 POINT BOUNTY
GLAD6	FDB	MSW12*256+PL2*$11,$4FB6	 GREEN - 3K POINT BOUNTY
EGG1	FDB	MSW13*256+WHI*$11,$387D	EGG WAVE INTRO
PTER1	FDB	MSW07*256+WHI*$11,$1178	1478	PTERODACTYL BEWARE
*
*	WAVE TABLE
* WAVE OFFSETS:
*  WBOUND  RMB     0       NBR OF BOUNDERS IN THIS WAVE (UPPER NIBBLE)
*  WHUNT   RMB     1       NBR OF HUNTERS IN THIS WAVE (LOWER NIBBLE)
*  WLORD   RMB     0       NBR OF SHADOW LORDS IN THIS WAVE (UPPER NIBBLE)
*  WPERSUE RMB     1       NBR OF PERSUERS IN THIS WAVE (LOWER NIBBLE)
*  WPTEN   RMB     1       NBR OF PTERODACTYLS IN THIS WAVE
*  WSTATUS RMB     1       STATUS BYTE FOR THIS WAVE
*
*	WAVE STATUS BYTE, BIT DEFINITION
*
* WBPER	  EQU	$01	IF=1, PERSUE WILL BE INSTANTANIOUSLY
* WBJSR0	EQU	$02	THESE 3 BITS ARE A JSR TABLE OFFSET
* WBJSR1	EQU	$04	 (0 = AN NOP, OTHERS ARE 2,4,6,8,10,12,14)
* WBJSR2	EQU	$08
* WBCL1L	EQU	$10	IF=1, CLIFF1L IS DISABLED (OR DESTROYED)
* WBCL1R	EQU	$20	IF=1, CLIFF1R IS DISABLED (OR DESTROYED)
* WBCL2 	EQU	$40	IF=1, CLIFF2 IS DISABLED (OR DESTROYED)
* WBCL4	  EQU	$80	IF=1, CLIFF4 IS DISABLED (OR DESTROYED)
* WBCL1	  EQU	WBCL1L+WBCL1R
* WBCL12	EQU	WBCL1+WBCL2
* WBCL14	EQU	WBCL1+WBCL4
* WBCL24	EQU	WBCL4+WBCL2
* WBCLS	  EQU	WBCL1+WBCL2+WBCL4

	FCB	$40,$01,0,0			WAVE 0 (FOR INITILIZATION)
WAVTBL

  IF Wave2EggWave
* Testing
	FCB	$60,$01,0,8			WAVE 5 - EGG WAVE
	FCB	$60,$01,0,8			WAVE 5 - EGG WAVE
	FCB	$60,$01,0,8			WAVE 5 - EGG WAVE


  FCB	$00,$AF,0,WBPER+WBCL4+6		WAVE 89- GLADIATOR WAVE
	FCB	$00,$8F,0,8			WAVE 80- EGG WAVE

  FCB	$00,$AF,0,WBPER+WBCL1+0		WAVE 81
	FCB	$00,$AF,0,WBPER+WBCL14+4	WAVE 82- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+WBCL14+10	WAVE 83- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCLS+6		WAVE 84- GLADIATOR WAVE
	FCB	$00,$8F,0,8			WAVE 85- EGG WAVE

	FCB	$00,$AF,0,WBPER+0		WAVE 86
	FCB	$00,$AF,0,WBPER+4		WAVE 87- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+10		WAVE 88- PTERODACTYL WAVE

  ENDIF

	FCB	$30,$01,0,2			WAVE 1 - NEEDS INTRO MESSAGE
	FCB	$40,$01,0,4			WAVE 2 - COOP/TEAM/SURVIVE WAVE
	FCB	$60,$02,0,0			WAVE 3
	FCB	$33,$01,0,6			WAVE 4 - GLADIATOR WAVE
	FCB	$60,$01,0,8			WAVE 5 - EGG WAVE

	FCB	$33,$03,0,WBPER+WBCL2+0		WAVE 6
	FCB	$24,$02,0,WBPER+WBCL12+4	WAVE 7 - COOP/TEAM/SURVIVE WAVE
	FCB	$06,$01,1,WBPER+WBCL12+10	WAVE 8 - PTERODACTYL WAVE
	FCB	$06,$02,0,WBPER+WBCLS+6		WAVE 9 - GLADIATOR WAVE
	FCB	$80,$03,0,8			WAVE 10- EGG WAVE

	FCB	$35,$03,0,WBPER+0		WAVE 11
	FCB	$26,$02,0,WBPER+WBCL4+4		WAVE 12- COOP/TEAM/SURVIVE WAVE
	FCB	$07,$03,1,WBPER+WBCLS+10	WAVE 13- PTERODACTYL WAVE
	FCB	$08,$04,0,WBPER+WBCLS+6		WAVE 14- GLADIATOR WAVE
	FCB	$06,$02,0,8			WAVE 15- EGG WAVE

	FCB	$05,$1F,0,0			WAVE 16
	FCB	$05,$1F,0,4			WAVE 17- COOP/TEAM/SURVIVE WAVE
	FCB	$05,$1F,2,10			WAVE 18- PTERODACTYL WAVE
	FCB	$04,$2F,0,6			WAVE 19- GLADIATOR WAVE
	FCB	$06,$03,0,8			WAVE 20- EGG WAVE

	FCB	$03,$3F,0,WBPER+WBCL1+0		WAVE 21
	FCB	$02,$4F,0,WBPER+WBCL1+4		WAVE 22- COOP/TEAM/SURVIVE WAVE
	FCB	$02,$4F,2,WBPER+WBCL14+10	WAVE 23- PTERODACTYL WAVE
	FCB	$02,$4F,0,WBPER+WBCL14+6	WAVE 24- GLADIATOR WAVE
	FCB	$06,$04,0,8			WAVE 25- EGG WAVE

	FCB	$03,$5F,0,WBPER+WBCLS+0		WAVE 26
	FCB	$03,$5F,0,WBPER+WBCLS+4		WAVE 27- COOP/TEAM/SURVIVE WAVE
	FCB	$02,$4F,2,WBPER+WBCLS+10	WAVE 28- PTERODACTYL WAVE
	FCB	$03,$5F,0,WBPER+WBCLS+6		WAVE 29- GLADIATOR WAVE
	FCB	$06,$03,0,8			WAVE 30- EGG WAVE

	FCB	$04,$4F,0,WBPER+0		WAVE 31
	FCB	$02,$6F,0,WBPER+4		WAVE 32- COOP/TEAM/SURVIVE WAVE
	FCB	$02,$4F,2,WBPER+WBCL2+10	WAVE 33- PTERODACTYL WAVE
	FCB	$02,$6F,0,WBPER+WBCL2+6		WAVE 34- GLADIATOR WAVE
	FCB	$08,$05,0,8			WAVE 35- EGG WAVE

	FCB	$02,$6F,0,WBPER+WBCL4+0		WAVE 36
	FCB	$00,$8F,0,WBPER+WBCL4+4		WAVE 37- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$6F,2,WBPER+WBCL24+10	WAVE 38- PTERODACTYL WAVE
	FCB	$00,$8F,0,WBPER+WBCL24+6	WAVE 39- GLADIATOR WAVE
	FCB	$08,$05,0,8			WAVE 40- EGG WAVE

	FCB	$03,$7F,0,WBPER+WBCL1+0		WAVE 41
	FCB	$00,$AF,0,WBPER+WBCL14+4	WAVE 42- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+WBCL14+10	WAVE 43- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCLS+6		WAVE 44- GLADIATOR WAVE
	FCB	$08,$06,0,8			WAVE 45- EGG WAVE

	FCB	$03,$7F,0,WBPER+0		WAVE 46
	FCB	$00,$AF,0,WBPER+4		WAVE 47- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+10		WAVE 48- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCL4+6		WAVE 49- GLADIATOR WAVE
	FCB	$08,$06,0,8			WAVE 50- EGG WAVE

	FCB	$03,$7F,0,WBPER+WBCL1+0		WAVE 51
	FCB	$00,$AF,0,WBPER+WBCL14+4	WAVE 52- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+WBCL14+10	WAVE 53- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCLS+6		WAVE 54- GLADIATOR WAVE
	FCB	$08,$0F,0,8			WAVE 55- EGG WAVE

	FCB	$03,$7F,0,WBPER+0		WAVE 56
	FCB	$00,$AF,0,WBPER+4		WAVE 57- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+010		WAVE 58- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCL4+6		WAVE 59- GLADIATOR WAVE
	FCB	$00,$63,0,8			WAVE 60- EGG WAVE

	FCB	$00,$AF,0,WBPER+WBCL1+0		WAVE 61
	FCB	$00,$AF,0,WBPER+WBCL14+4	WAVE 62- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+WBCL14+10	WAVE 63- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCLS+6		WAVE 64- GLADIATOR WAVE
	FCB	$00,$64,0,8			WAVE 65- EGG WAVE

	FCB	$00,$AF,0,WBPER+0		WAVE 66
	FCB	$00,$AF,0,WBPER+4		WAVE 67- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+10		WAVE 68- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCL4+6		WAVE 69- GLADIATOR WAVE
	FCB	$00,$65,0,8			WAVE 70- EGG WAVE

	FCB	$00,$AF,0,WBPER+WBCL1+0		WAVE 71
	FCB	$00,$AF,0,WBPER+WBCL14+4	WAVE 72- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+WBCL14+10	WAVE 73- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCLS+6		WAVE 74- GLADIATOR WAVE
	FCB	$00,$6F,0,8			WAVE 75- EGG WAVE

	FCB	$00,$AF,0,WBPER+0		WAVE 76
	FCB	$00,$AF,0,WBPER+4		WAVE 77- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+10		WAVE 78- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCL4+6		WAVE 79- GLADIATOR WAVE
	FCB	$00,$86,0,8			WAVE 80- EGG WAVE

WTBRST	FCB	$00,$AF,0,WBPER+WBCL1+0		WAVE 81
	FCB	$00,$AF,0,WBPER+WBCL14+4	WAVE 82- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+WBCL14+10	WAVE 83- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCLS+6		WAVE 84- GLADIATOR WAVE
	FCB	$00,$8F,0,8			WAVE 85- EGG WAVE

	FCB	$00,$AF,0,WBPER+0		WAVE 86
	FCB	$00,$AF,0,WBPER+4		WAVE 87- COOP/TEAM/SURVIVE WAVE
	FCB	$00,$7F,3,WBPER+10		WAVE 88- PTERODACTYL WAVE
	FCB	$00,$AF,0,WBPER+WBCL4+6		WAVE 89- GLADIATOR WAVE
	FCB	$00,$8F,0,8			WAVE 80- EGG WAVE
WTBEND	EQU	*			END OF WAVE TABLE, REPEAT AT "WTBRST"
*
*	ALTERNATE COPYRIGHT MESSAGE
*
MSCOP2	FDB	$10F8
;	FCB	$2A!XHIDE+HIDE2,$11	(
;	FCB	$0D!XHIDE+HIDE2	C
;	FCB	$2B!XHIDE+HIDE2	)
;	FCB	$0A!XHIDE+HIDE2	SPACE
;	FCB	$21!XHIDE+HIDE2	W
;	FCB	$13!XHIDE+HIDE2	I
;	FCB	$16!XHIDE+HIDE2	L
;	FCB	$16!XHIDE+HIDE2	L
;	FCB	$13!XHIDE+HIDE2	I
;	FCB	$0B!XHIDE+HIDE2	A
;	FCB	$17!XHIDE+HIDE2	M
;	FCB	$1D!XHIDE+HIDE2	S
;	FCB	$0A!XHIDE+HIDE2	SPACE
;	FCB	$0F!XHIDE+HIDE2	E
;	FCB	$16!XHIDE+HIDE2	L
;	FCB	$0F!XHIDE+HIDE2	E
;	FCB	$0D!XHIDE+HIDE2	C
;	FCB	$1E!XHIDE+HIDE2	T
;	FCB	$1C!XHIDE+HIDE2	R
;	FCB	$19!XHIDE+HIDE2	O
;	FCB	$18!XHIDE+HIDE2	N
;	FCB	$13!XHIDE+HIDE2	I
;	FCB	$0D!XHIDE+HIDE2	C
;	FCB	$1D!XHIDE+HIDE2	S
;	FCB	$0A!XHIDE+HIDE2	SPACE
;	FCB	$13!XHIDE+HIDE2	I
;	FCB	$18!XHIDE+HIDE2	N
;	FCB	$0D!XHIDE+HIDE2	C
;	FCB	$2E!XHIDE+HIDE2	.
;	FCB	0

	FCB	$A0
	FCB	$11
	FCB	$87
	FCB	$A1
	FCB	$80
	FCB	$AB
	FCB	$79
	FCB	$7C
	FCB	$7C
	FCB	$79
	FCB	$81
	FCB	$7D
	FCB	$77
	FCB	$80
	FCB	$85
	FCB	$7C
	FCB	$85
	FCB	$87
	FCB	$74
	FCB	$76
	FCB	$73
	FCB	$72
	FCB	$79
	FCB	$87
	FCB	$77
	FCB	$80
	FCB	$79
	FCB	$72
	FCB	$87
	FCB	$A4
	FCB	$00


*
*	WAVE SUBROUTINE TABLE
*
WJSRTB	FDB	WAVRTS		NOP ROUTINE
	FDB	WINTRO		INTRODUCTION MESSAGE
	FDB	WCOOP		CO-OPERATION WAVE SET-UP
	FDB	WGLAD		GLADIATOR SET-UP
	FDB	WAVEGG		EGG WAVE SET-UP
	FDB	WPTERO		PTERODACTYL WAVE
*
WAVRTS	RTS			NOTHING TO DO
*
WINTRO	LDX	#INTRO1
	JSR	WAVMSG
	LDA	#90/6
	JSR	WAVDEL
	LDX	#INTRO2
	JSR	WAVMSG
	LDA	#90/6		RESET OF 1 SECOND DELAY
	JMP	WPAUS2
*
*	PTERODACTYL WAVE START
*
WPTERO	EQU	*		PTERODACTYL WAVE
	LDX	#PTER1
	JSR	WAVMSG
	LDA	#180/6
	JSR	WAVDEL
	JSR	WATWAV		WAIT TILL TRUE END OF WAVE
	SECCR	PTERWV,PTEID	START WAVE STARTED PTERODACYL'S
	LDX	PWAVE,U		GET WAVE AREA TO WORK FROM
	LDA	WPTEN,X		 GET NBR OF PTERODCTYLS FOR THIS WAVE
	STA	PJOYT,Y		NUMBER OF PTERODACTYLS TO CREATE
	JMP	WPAUS3
*
PTERWV	PCNAP	65
	DEC	PJOYT,U		NUMBER OF PTERO'S TO CREATE
	BLE	L001_023		 BR=LAST ONE
	SECCR	PTERST,PTEID
	BRA	PTERWV
*
L001_023:	JMP	PTERST
*
*	CO-OPERATION (TEAM) WAVE OR SURVIVAL WAVE (2 PLAYER/1 PLAYER)
*
WCOOP	LDA	SPLY2+6		THIS MESSAGE IS FOR TWO PLAYERS ONLY
	BEQ	WAVSUR		 BR=PLAYER 2 DEAD, GO TO SURVIVAL WAVE
	LDA	SPLY1+6
	BEQ	WAVSUR		 BR=PLAYER 1 DEAD, GO TO SURVIVAL WAVE
	LDD	#WCOSCR		CO-OPERATION WAVE SET-UP
	STD	PWPREV,U		CO-OP SCORE ROUTINE
	CLR	PLYG1		RESET PLAYER HIT PLAYER COUNTER(S)
	CLR	PLYG2
	LDX	#COOP1
	JSR	WAVMSG
	LDX	#COOP2
	JMP	WAVMSG
*
WCOSCR	EQU	*		END OF WAVE CLEAN-UP
	LDA	PLYG1		PLAYER 1 OR
	ORA	PLYG2		PLAYER 2 DIE FROM ONE ANOTHER?
	BEQ	L001_024		 BR=NO, GO AWARD THE TEAM
	LDX	#COOP3		NO BONUS POINTS MESSAGE
	BRA	ENDTS
*
L001_024:	LDA	SPLY1+6		PLAYER 1 IS STILL ALIVE?
	BEQ	L002_014		 BR=NO, DEAD
	LDX	#P1DEC		AWARD POINTS TO PLAYER 1
	LDA	#$30		 3,000 POINTS!!!
	JSR	SCRHUN
L002_014:	LDA	SPLY2+6		PLAYER 2 IS STILL ALIVE?
	BEQ	L003_006		 BR=NO, DEAD
	LDX	#P2DEC		AWARD POINTS TO PLAYER 2
	LDA	#$30		 3,000 POINTS!!!
	JSR	SCRHUN
L003_006:	LDX	#COOP4		BONUS POINTS MESSAGE
ENDTS	JSR	WAVMSG
	LDA	#120/6		WAIT FOR 2 SECONDS
	JMP	WAVDEL		THEN RETURN TO BEGIN NEW WAVE
*
*	SURVIVAL WAVE (ASSUMES ONLY 1 PLAYER LEFT IN GAME)
*
WAVSUR	LDD	#WSUSCR		END OF SURVIVAL WAVE
	STD	PWPREV,U	 SCORE ROUTINE
	CLR	PLYD1		RESET PLAYER DEATH COUNTER(S)
	CLR	PLYD2
	LDX	#SURV1
	JMP	WAVMSG
*
*	END OF SURVIVAL WAVE SCOREING
*
WSUSCR	LDX	#SURV3		ASSUME NO POINTS
	LDA	SPLY1+6		STILL HAVE SOME LIVES LEFT?
	BEQ	L001_025		 BR=NO, CK PLAYER 2
	LDA	PLYD1		DID PLAYER 1 DIE?
	BNE	ENDTS		 BR=YES, NO SCORE
	LDX	#P1DEC		AWARD POINTS TO PLAYER 1
	LDA	#$30		 3,000 POINTS!!!
	JSR	SCRHUN
	LDX	#SURV4		AWARDED POINTS MESSAGE	(PLAYER 1)
	BRA	ENDTS
*
L001_025:	LDA	SPLY2+6		STILL HAVE SOME LIVES LEFT?
	BEQ	ENDTS		 BR=NO, NO BONUS AWARDED
	LDA	PLYD2		DID PLAYER 2 DIE?
	BNE	ENDTS		 BR=YES, NO SCORE
	LDX	#P2DEC		AWARD POINTS TO PLAYER 2
	LDA	#$30		 3,000 POINTS!!!
	JSR	SCRHUN
	LDX	#SURV5		AWARDED POINTS MESSAGE	(PLAYER 5)
	BRA	ENDTS
*
*	START GLADIATOR WAVE
*
WGLAD	LDA	SPLY1+6		THIS MESSAGE IS FOR TWO PLAYERS ONLY
	BEQ	WAVRT2		 BR=PLAYER 1 DEAD
	LDA	SPLY2+6
	BEQ	WAVRT2		 BR=PLAYER 2 DEAD
	LDD	#WGLSCR		GLADIATOR SET-UP
	STD	PWPREV,U		TO SCORE ROUTINE
	LDA	#-1		SET-UP PLAYER KILLING PLAYER VARIABLES
	STA	PLYG1		 TO AWARD SCORE UPON 1ST ENCOUNTER
	STA	PLYG2
	LDX	#GLAD1
	JSR	WAVMSG
	LDA	#90/6
	JSR	WAVDEL
	LDX	#GLAD2
	JSR	WAVMSG
	LDX	#GLAD3
	JSR	WAVMSG
	LDA	#90/6
	JMP	WPAUS2
WAVRT2	RTS
*
*	END OF GLADIATOR WAVE CLEAN-UP
*
WGLSCR
	LDA	PLYG1		EITHER BOTH NEGATIVE, OR 1 IS, NO OPTIONS
	BPL	L002_015		 BR=PLAYER 2 GOT THE BOUNTY
	LDX	#GLAD5		PLAYER 1 BONUS POINTS MESSAGE
	LDA	PLYG2
	BPL	L003_007		BR= PLAYER 1 GOT THE BOUNTY
	LDX	#GLAD4		NO ONE GOT THE BOUNTY
	BRA	L003_007
L002_015:	LDX	#GLAD6		PLAYER 2 BONUS POINTS MESSAGE
L003_007:	JSR	WAVMSG
	CLR	PLYG1		NO MORE AWARDING BOUNTY POINTS
	CLR	PLYG2
	LDA	#120/6		WAIT FOR 2 SECONDS
	JMP	WAVDEL		THEN RETURN TO BEGIN NEW WAVE
*
*	START OF EGG WAVE
*
WAVEGG	LDX	#EGG1		EGG WAVE
	JSR	WAVMSG
	LDA	#180/6
	JSR	WAVDEL
	JSR	WATWAV		WAIT TILL TRUE END OF WAVE
	JSR	EMSGS		ERASE AL MESSAGES
*
	LDX	PWAVE,U		FIND INTELLIGENCE
	LDY	#P6DEC		ASSUME THE BEST
	LDA	WBOUND,X
	BNE	L001_026
	LDA	WLORD,X		IT IS THE LORD HIMSELF
	BRA	L002_016
L001_026:	LDY	#P5DEC		HUNTER?
	BITA	#$0F
	BNE	L003_008		 BR=YES
	LDY	#P4DEC		HO, HUM, THE BOUNDAR
L002_016:	LSRA
	LSRA
	LSRA
	LSRA
L003_008:	ANDA	#$0F
	STA	WENEMY		NUMBER OF ENEMIES TO HATCH AT A TIME
	STY	PINTEL,U	START INTELLIGENCE OF ALL
	LDB	EGGWT2		INITIAL EGG WAITING TIME
	LDA	SPLY2+6		2 PLAYERS IN THE GAME
	BEQ	L035_001		 BR=NO, ALL TIMES ARE BASIED ON 1 PLAYER
	LDA	SPLY1+6		2 PLAYERS IN THE GAME
	BEQ	L035_001		 BR=YES, ALL TIMES ARE BASIED ON 1 PLAYER
	LSRB			ON 2 PLAYERS USE 1/2 OF THE TIME
L035_001:	STB	PEGGTM,U	EGG TIMER
	LDA	WPERSUE,X	NBR TO START PERSUING
	ANDA	#$0F
	STA	WSMART
	LDX	#EGPTBL		CLEAR EGG PLACEMENT TABLE
L004_005:	CLR	,X+
	CMPX	#EGLEND		& CLEAR LEVEL RANDOMNESS
	BLO	L004_005
*
	LDA	#2		NUMBER OF PRE-MATURE EGG HATCHINGS
	STA	PWHCH,U
	LDA	#6
	STA	PWREGA,U	6 EGGS ON EACH LEDGE
L005_004:	LDY	#EGLEDG		PUT AN EGG ON EVERY LEDGE (RANDOM LEDGE)
	LDX	#EGLTBL
	JSR	VRAND		SELECT RANDOM LEDGE
	LDB	#6*2
	MUL
L054_000:	LDB	A,X
	BEQ	L055_001
	DECA
	BPL	L054_000
	LDA	#6-1
	BRA	L054_000
*
L055_001:	INC	A,X
	LEAY	A,Y
	JSR	VRAND
	ROLA			GET RANDOM NBR 0 TO 255
	LDB	,Y+
	SUBB	-2,Y
	MUL
	ADDA	-2,Y
	LDX	#EGPTBL		(EGG PLACEMENT TABLE)
	INC	A,X		THIS SPOT HAS BEEN TAKEN!
	JSR	CREGG		CREATE THE EGG
	DEC	PWREGA,U	ANY MORE EGGS LEFT
	BNE	L005_004		 BR=NO
	LDA	#6
	STA	PWREGA,U	6 MORE EGGS TO GO
L006_002:	LDX	#EGPTBL		POINT TO EGG POSITION TABLE
	JSR	VRAND
	LDB	#69*2
	MUL			REG. A = RANDOM NBR 0-68
L007_001:	LDB	A,X		IS THIS LOCATION FREE?
	BEQ	L008_001		 BR=YES
	INCA			NEXT LOCATION
	CMPA	#69		AND WRAP AROUND 69
	BLO	L007_001
	CLRA
	BRA	L007_001
*
L008_001:	INC	A,X		THIS SPOT RESERVED
	JSR	CREGG		CREATE THE EGG
	DEC	PWREGA,U	ANY MORE LEFT?
	BNE	L006_002		 BR=YES
	JMP	WBEGIN
*
*	TROMSG
*
TROMSG	; All commented out with actual FCB values below this section
;	FCB	($1E+$35)!X$9B,($12+$35)!X$9B,($13+$35)!X$9B,($1D+$35)!X$9B
;	FCB	 ($0A+$35)!X$9B,($13+$35)!X$9B,($1D+$35)!X$9B,($0A+$35)!X$9B
;	FCB	 ($14+$35)!X$9B,($19+$35)!X$9B,($1F+$35)!X$9B,($1D+$35)!X$9B
;	FCB	 ($1E+$35)!X$9B,($2E+$35)!X$9B,($00+$35)!X$9B
;	FCB	($0E+$35)!X$9B,($0F+$35)!X$9B,($1D+$35)!X$9B,($13+$35)!X$9B
;	FCB	 ($11+$35)!X$9B,($18+$35)!X$9B,($0F+$35)!X$9B,($0E+$35)!X$9B
;	FCB	 ($0A+$35)!X$9B,($0C+$35)!X$9B,($23+$35)!X$9B,($0A+$35)!X$9B
;	FCB	 ($21+$35)!X$9B,($13+$35)!X$9B,($16+$35)!X$9B,($16+$35)!X$9B
;	FCB	 ($13+$35)!X$9B,($0B+$35)!X$9B,($17+$35)!X$9B,($1D+$35)!X$9B
;	FCB	 ($0A+$35)!X$9B,($0F+$35)!X$9B,($16+$35)!X$9B,($0F+$35)!X$9B
;	FCB	 ($0D+$35)!X$9B,($1E+$35)!X$9B,($1C+$35)!X$9B,($19+$35)!X$9B
;	FCB	 ($18+$35)!X$9B,($13+$35)!X$9B,($0D+$35)!X$9B,($1D+$35)!X$9B
;	FCB	 ($0A+$35)!X$9B,($13+$35)!X$9B,($18+$35)!X$9B,($0D+$35)!X$9B
;	FCB	 ($2E+$35)!X$9B,($00+$35)!X$9B
;	FCB	($2A+$35)!X$9B,($0D+$35)!X$9B,($2B+$35)!X$9B
;	FCB	 ($0A+$35)!X$9B,($01+$35)!X$9B,($09+$35)!X$9B
;	FCB	 ($08+$35)!X$9B,($02+$35)!X$9B,($0A+$35)!X$9B,($21+$35)!X$9B
;	FCB	 ($13+$35)!X$9B,($16+$35)!X$9B,($16+$35)!X$9B,($13+$35)!X$9B
;	FCB	 ($0B+$35)!X$9B,($17+$35)!X$9B,($1D+$35)!X$9B,($0A+$35)!X$9B
;	FCB	 ($0F+$35)!X$9B,($16+$35)!X$9B,($0F+$35)!X$9B,($0D+$35)!X$9B
;	FCB	 ($1E+$35)!X$9B,($1C+$35)!X$9B,($19+$35)!X$9B,($18+$35)!X$9B
;	FCB	 ($13+$35)!X$9B,($0D+$35)!X$9B,($1D+$35)!X$9B,($0A+$35)!X$9B
;	FCB	 ($13+$35)!X$9B,($18+$35)!X$9B,($0D+$35)!X$9B,($2E+$35)!X$9B
;	FCB	 ($00+$35)!X$9B
;	FCB	($0B+$35)!X$9B,($16+$35)!X$9B,($16+$35)!X$9B,($0A+$35)!X$9B
;	FCB	 ($1C+$35)!X$9B,($13+$35)!X$9B,($11+$35)!X$9B,($12+$35)!X$9B
;	FCB	 ($1E+$35)!X$9B,($1D+$35)!X$9B,($0A+$35)!X$9B,($1C+$35)!X$9B
;	FCB	 ($0F+$35)!X$9B,($1D+$35)!X$9B,($0F+$35)!X$9B,($1C+$35)!X$9B
;	FCB	 ($20+$35)!X$9B,($0F+$35)!X$9B,($0E+$35)!X$9B,!W($80+$35!X$9B)

; Actual Values of list above
	FCB		$C8,$DC,$D3,$C9
	FCB		$A4,$D3,$C9,$A4
	FCB		$D2,$D5,$CF,$C9
	FCB		$C8,$F8,$AE,$D8
	FCB		$DF,$C9,$D3,$DD
	FCB		$D6,$DF,$D8,$A4
	FCB		$DA,$C3,$A4,$CD
	FCB		$D3,$D0,$D0,$D3
	FCB		$DB,$D7,$C9,$A4
	FCB		$DF,$D0,$DF,$D9
	FCB		$C8,$CA,$D5,$D6
	FCB		$D3,$D9,$C9,$A4
	FCB		$D3,$D6,$D9,$F8
	FCB		$AE,$C4,$D9,$FB
	FCB		$A4,$AD,$A5,$A6
	FCB		$AC,$A4,$CD,$D3
	FCB		$D0,$D0,$D3,$DB
	FCB		$D7,$C9,$A4,$DF
	FCB		$D0,$DF,$D9,$C8
	FCB		$CA,$D5,$D6,$D3
	FCB		$D9,$C9,$A4,$D3
	FCB		$D6,$D9,$F8,$AE
	FCB		$DB,$D0,$D0,$A4
	FCB		$CA,$D3,$DD,$DC
	FCB		$C8,$C9,$A4,$CA
	FCB		$DF,$C9,$DF,$CA
	FCB		$CE,$DF,$D8,$2E
*
*	CREATE AN EGG ON THE GROUND
*	 INPUT	REG.A, LEDGE POSITION ( A NBR FROM 0-65)
*		PINTEL,U = START INTELLIGENCE
*		PEGGTM = TIME TILL HATCHING
*	 NO REGISTERS ARE SAVED
*
CREGG	PSHS	A
	INC	NRIDER		1 MORE RIDER (EGG) IN THE GAME
	SECCR	EGGLN2,$80+EGGID
	PULS	A
	LDX	#EGGLNT-4
L001_027:	LEAX	4,X
	CMPA	,X
	BHS	L001_027
	LDB	#8		PUT AN EGG EVERY 8TH PIXEL POSITION
	STB	PFEET,Y		(NO CAUGHT IN THE AIR POINTS)
	MUL
	ADDD	2,X		NEW X POSITION
	STD	PPOSX,Y
	ADDD	#8-1-1
	STD	PCOLX,Y		MAINTAIN X-DIRECTION COLISION DETECT POINTERS
	LDB	1,X
	CLRA
	STD	PPOSY,Y		NEW Y POSITION
	STB	PCOLY1,Y	MAINTAIN Y-DIRECTION COLISION DETECT POINTERS
	SUBB	#7-1
	STB	PCOLY2,Y
	LDD	PINTEL,U	NEW INTELLIGENCE
	STD	PDECSN,Y
	LDB	PEGGTM,U	HATCHING TIME
	STB	PJOYT,Y
	DEC	PWHCH,U		ANYMORE EGGS TO HATCH PREMATURLY?
	BMI	L020_018
	JSR	VRAND		A RANDOM NUMBER 0-127
	MUL			GET A RANDOM TIME 1/2 OF THE RANGE
	NEGA
	ADDA	PJOYT,Y
	STA	PJOYT,Y		NEW HATCHING TIME
L020_018:	LDD	#0
	STD	PPICH,Y		NO IMAGE TO CLEAR
	STD	PDIST,Y		NO BUZZARD SENT AFTER EGG
********	STD	PVELY,Y		ALL EGGS STRAIGHT UP?
	CLR	PCHASE,Y	NOT CHASEING ANYONE
	LDA	#4		4 EGGS BEFORE ENEMY R.I.P.'S
	STA	PEGG,Y
	LDD	,S++		GET RETURN ADDRESS
	STD	PWRTS,U
	PCNAP	1		DELAY FOR OUT OF PHASE EGG HATCHING & MONITOR
	JMP	[PWRTS,U]	RETURN
*
*	PUT UP AN EGG ON EVERY LEDGE, LEDGE EGG LENGTH DEFINITIONS
*
	FCB	0
EGLEDG	FCB	3+5				LEVEL0 - CLIF1L, CLIF1R
	FCB	3+5+11				LEVEL1 - CLIF2
	FCB	3+5+11+7			LEVEL2 - CLIF3U
	FCB	3+5+11+7+7+5			LEVEL3 - CLIF3L, CLIF3R
	FCB	3+5+11+7+7+5+8			LEVEL4 - CLIF4
	FCB	3+5+11+7+7+5+8+23		LEVEL5 - CLIF5
*
*	EGG LAND DESCRIPTION
*
EGGLNT
	FCB	3,$45-1			CLIF1L
	FDB	$00+9
	FCB	3+5,$45-1		CLIF1R
	FDB	$F4-8*(3)+9
	FCB	3+5+11,$51-1		CLIF2
	FDB	$4D-8*(3+5)+9
	FCB	3+5+11+7,$81-1		CLIF3U
	FDB	$C1-8*(3+5+11)+9
	FCB	3+5+11+7+7,$8A-1	CLIF3L
	FDB	$00-8*(3+5+11+7)+9
	FCB	3+5+11+7+7+5,$8A-1	CLIF3R (EXTRA LONG STYLE)
	FDB	$100-8*(3+5+11+7+7)+1
	FCB	3+5+11+7+7+5+8,$A3-1	CLIF4
	FDB	$61-8*(3+5+11+7+7+5)+9
	FCB	3+5+11+7+7+5+8+23,$D3-1	CLIF5
	FDB	$2D-8*(3+5+11+7+7+5+8)+9
	FCB	$FF,$45-1		CATCH ALL
	FDB	$20-8*(3+5+11+7+7+4+8+23)+9
*
*	PTERODACTYL DIES
*
DEATH4	PSHS	X,U	SAVE (REG.X VICTOR, & REG.U DEAD GUY)
	LDA	#8-1
	STA	PJOYT,U
	LDD	#PTEKLL		SHOW A DEAD PTERODACTYL!!!!!!!!!!!!!
	STD	PJOY,U
	LDX	#SNPTED		PTERODACTYL DYING SOUND
	JSR	VSND
	PULS	X,U,PC
*
*	ENEMY DIES
*
DEATH3	PSHS	X,U	SAVE (REG.X VICTOR, & REG.U DEAD GUY)
	LDX	#SNEDIE		ENEMY DIES
	JSR	VSND
	LDA	NSMART		BUMP DOWN SMARTNESS VARIABLE
	SUBA	PCHASE,U
	STA	NSMART
	DEC	NENEMY		DECREMENT NBR OF ENEMIES ON THE SCREEN
	LDA	#$80+EGGID	START EGG I.D.
	LDB	PPRI,U		EGG IS SAME PRIMARY/SECONDARY PROCESS
	LDX	#STEGG
	JSR	VCUPROC
	LDD	#0		NO BIRD IS AFTER THE LITTLE MAN
	STD	PDIST,Y
	STD	PPICH,Y		NO IMAGE TO CLEAR
	STD	PCOLY1,Y	& PCOLY2, INHIBIT EARLY COLISION
	LDD	PPOSY,U		ADJUST FOR BOTTOM OF EGG FROM
	SUBB	#13-5		 BOTTOM OF HORSE (PLUS MIKE STROLL FUDGE
	STD	PPOSY,Y			FACTOR TO MISS THE EGG)
	LDD	PPOSX,U
	ADDD	#4
	STD	PPOSX,Y
	CLR	PFEET,Y		NOT CAUGHT IN THE AIR YET! (500 PTS. BONUS)
	LDD	PDECSN,U	TRANSFER DECISION TABLE TO EGG
	CMPD	#P6DEC		MOST DIFFICULT ENEMY?
	BHS	DE3HRD		 BR=YES
	ADDD	#P4DEC-P3DEC	NEXT LEVEL OF INTELLIGENCE
DE3HRD	STD	PDECSN,Y
	LDD	PVELY,U		SAME VELOCITIES
	STD	PVELY,Y
	LDA	PVELX,U
	STA	PVELX,Y
	CLR	PBUMPX,Y	RESET BUMPAGE REGISTERS
	CLR	PBUMPY,Y
	CLR	PTIMX,Y
	CLR	PFRAME,Y
	LDA	PEGG,U		TRANSFER NBR OF EGG LEFT
	STA	PEGG,Y
	DEC	PEGG,Y		YOU CAN ONLY SQUEEZE SO MUCH BLOOD FROM AN EGG
	BNE	L001_029		 BR=YOU CAN GET MORE EGGS
	TFR	Y,X
	LDU	,S		GET VICTORS WORKSPACE (CURRENTLY DIEING ENEMY)
	BEQ	L001_029
	JSR	EGGSCR		SCORE EGG
L001_029:	PULS	X,U,PC
*
PLYEGG	EQU	*		COLISION DETECT WITH THE EGG
	JSR	EGGSCR		COLLECT THE EGGS SCORE VALUE
	JMP	HITEM2		END OF COLISION WITH EGG
*
*	SCORE THE EGG
*	 INPUT REG.U, THE PLAYER'S (VICTOR) WORKSPACE THAT HIT THE EGG
*	 	REG.X, THE EGG'S WORKSPACE
*	 OUTPUT - THE EGG'S WORKSPACE TO DISPLAY SCORE VALUE
*		  PJOYT,PPVELX = MESSAGE & COLOR
*
EGGSCR	PSHS	X,U
	LDX	#SNEGG		EGG & PLAYER COLIDE, IF HERE
	JSR	VSND
	LDY	PDECSN,U	FIND INCREMENTING SCORE VALUE FOR EGG
	LDA	DCONST,Y	GET SCORES COLOR
	LDX	,S		RESTORE EGG'S WORKSPACE
	STA	PPVELX,X	SAVE IT IN EGGS PPVELX AREA
	LDY	DEGGS,Y
EGGDB1	LDB	,Y		GET NUMBER OF TIME AN EGG WAS HIT
	CMPB	#4		THE MAGIC MAXIMUM NUMBER IS 4
	BHS	EGGSMN
L001_030:	INCB
EGGSMN	STB	,Y
	STB	PRDIR,X		SAVE SCORE VALUE IN EGGS PRDIR WORKSPACE
	ASLB			MULTIPLY BY 3
	ADDB	,Y
	LDY	#EGGVAL-3		EGG SCORE TABLE
	LEAY	B,Y
	LDA	,Y		GET SCORE VALUE
	LDX	PDECSN,U	SCORE IN CORRECT AREA
	JSR	[1,Y]		SCORE POINTS WITH PROPER SCORE ROUTINE
*
	LDY	,S		CAUGHT IN THE AIR BEFORE BOUNCING?
	CLR	PEGG,Y		ASSUME NO ADDITIONAL SCORE
	LDA	PFEET,Y
	BNE	L001_031		 BR=NO
	COM	PEGG,Y		INDICATE ADDITIONAL SCORE
	LDA	#$05		SCORE 500 POINTS
	JSR	SCRHUN		 (IN PLAYERS WORKSPACE)
*
L001_031:	DEC	NRIDER		1 LESS ENEMY RIDER
	LDX	,S
	LDY	PDIST,X		WAS A BIRD AFTER THE LITTLE MAN?
	BEQ	EGGWAK		 BR=NO
	DEC	NENEMY		THIS GUY IS NO LONGER AN ENEMY
	LDD	#AUTOFF		THE BIRD SHOULD GO OFF SCREEN
	STD	PJOY,Y
EGGWAK	LDD	#EGGHIT		WAKE-UP AT HIT EGG AREA
	STD	PPC,X
	LDA	#4
	STA	PNAP,X
	LDA	PID,X		NO LONGER COLIDE WITH EGG
	ANDA	#$7F
	STA	PID,X
	PULS	X,U,PC
*
EGGVAL	FCB	$52		250
	 FDB	SCRTEN
	FCB	$05		500
	 FDB	SCRHUN
	FCB	$57		750
	 FDB	SCRTEN
	FCB	$10		PEG AT 1,000
	 FDB	SCRHUN
*
*	GRAVITY FOR THE EGG
*	 INPUT REG.B ADDITION TO GRAVITY (VELOCITY)
*	 1 - MODIFY VELOCITY Y FOR GRAVITY
*	 2 - CALC NEW POSITIONS
*
ADDEGG	LDB	GRAV		ADD IN VARIABLE GRAVITY
	SEX
	ADDD	PVELY,U
	STD	PVELY,U		NEW Y VELOCITY
	ADDD	PPOSY+1,U	ADD IN FRACTIONAL DISTANCE
	ADDA	PBUMPY,U	ADD IN BUMPING REGISTER
	CLR	PBUMPY,U
	CMPA	#CEILNG		HIGHEST POSITION *****
	BHI	ADECEI		 BR=HAVE NOT HIT CEILING
	INC	PBUMPY,U
	LDD	PVELY,U
	BPL	ADEDWN		 BR=ALREADY GOING DOWN, DON'T GO UP
	COMA
	NEGB
	SBCA	#-1		INVERT VELOCITY
	STD	PVELY,U
ADEDWN	LDD	#CEILNG*256
ADECEI	STD	PPOSY+1,U
	STA	PCOLY1,U	MAINTAIN Y-DIRECTION COLISION DETECT POINTERS
	SUBA	#7-1
	STA	PCOLY2,U
*
	LDA	PVELX,U		MODIFY X DIRECTION
	LDX	#FLYX
	LDD	A,X
	ADDB	PVELX+2,U	ADD IN FRACTIONAL DISTANCE
	STB	PVELX+2,U
	ADCA	#0		CARRY TO POSITION X
	TFR	A,B		NEW DELTA DISTANCE
	JSR	WRAPX		ADD IN BUMPAGE REG. & NORMAL WRAP AROUND
	CMPD	#288		NEW WRAP AROUND STYLE (4 TO 288)
	BLE	EGGWR3
	ADDD	#4-288
EGGWR3	CMPD	#4
	BGE	EGGWR2
	ADDD	#288-4
EGGWR2	STD	PPOSX,U		FINAL POSITION
	ADDD	#8-1-1
	STD	PCOLX,U		MAINTAIN COLISION DETECT POINTERS
	RTS
*
*	EGG FALLING & BOUNCE ROUTINE
*
STEGG	EQU	*
EGGLPA	JSR	WEGG
	LDA	EMYTIM		ENEMY FALL TIME
	JSR	VNAPTPC
	JSR	CEGG
	JSR	ADDEGG		FALL DOWN
*
	JSR	EGGBKG		REFRESH BACKGROUND
	LDX	PPOSX,U		HAS THE EGG LANDED?
	LDY	PPOSY,U
	LDA	LNDXTB,X
	ORA	LNDXTB-2,X
	ANDA	LNDXTB-9,X
	ANDA	LNDYTB,Y
	ANDA	#$7F		IGNORE LAVA TROLL
	BNE	EGGBON		BR=EGG HIT TOP OF LAND, BOUNCE IT
EGGBCK
	LDX	PPOSX,U		HAS THE EGG HIT THE BACKGROUND?
	LDY	PPOSY,U		NEED THIS IN CASE OF RE-ADJUSTED LANDING
	LDA	BCKXTB,X	HIT BACKGROUND CLIFF?
	ANDA	BCKXTB-10,X
	ANDA	BCKYTB+6,Y
	ANDA	BCKYTB+13,Y
	BEQ	L001_032
	JSR	BCKCOL		COLLIDE WITH BACKGROUND
L001_032:	LDB	PPOSY+1,U
	CMPB	#FLOOR+4	EGG BELOW THE FLOOR? (UGH, WHAT A MESS)
	BLO	EGGLPA		 BR=NO
	DEC	NRIDER		1 LESS RIDER IN THE GAME
	JMP	EGGDN2		SCRAMBLED EGG IS FINISHED
*
EGGBON	STA	PFEET,U		NO MORE EGG IN THE AIR BOUNS POINTS
	JSR	LND18		CORRECT FOR TOP OF CLIFF
	STB	PCOLY1,U	RE-ADJUST COLISION POINTERS
	SUBB	#7-1
	STB	PCOLY2,U
	LDA	PVELX,U
	BEQ	EGGVX0		BR=NO X VELOCITY
	BMI	EGGVXM
	ADDA	#-2
	BRA	EGGVX0
*
EGGVXM	ADDA	#2
EGGVX0	STA	PVELX,U
	LDD	PVELY,U
	BMI	EGGBCK
	ASRA
	RORB
	ASRA
	RORB
	COMA
	NEGB
	SBCA	#-1
	STD	PVELY,U
	CMPD	#-$0020
	BLT	EGGBCK		BR=STILL TOO FAST TO LAND
	LDA	PVELX,U
	BNE	EGGBCK		BR=STILL TOO FAST TO LAND
*
EGGLND	LDA	EGGWT		GET CURRENT WAIT TIME
	STA	PJOYT,U
EGGLN2	JSR	WEGG
	PCNAP	12		12 EGG MAXIMUM, HOPEFULLY MULTIPLEXED
	LDX	PPOSX,U		IS THE EGG STILL ON LAND? (BURNING BRIDGE)
	LDY	PPOSY,U
	LDA	LNDXTB,X
	ORA	LNDXTB-2,X
	ANDA	LNDXTB-9,X
	ANDA	LNDYTB+1,Y	(FUDGE Y-POSITION FOR STILL ON LAND CHECK)
	ANDA	#$7F		IGNORE LAVA TROLL
	LBEQ	STEGG		BR=LAND HAS DISAPPEARED, MAKE IT FALL
	DEC	PJOYT,U
	BNE	EGGLN2
	INC	PJOYT,U		SET = 1,
	LDA	NENEMY		ENOUGH ENEMIES IN THIS WAVE?
	CMPA	WENEMY
	BHS	EGGLN2
	INC	NENEMY		1 MORE ENEMY COMMING UP
	LDX	#SNEGGH		MAKE THE SOUND OF AN EGG HATCHING
	JSR	VSND
	LDA	#EMYID		SIGNAL BUZARD TO  START FLYING IN!
	LDB	PPRI,U
	LDX	#STFLY2
	JSR	VCUPROC		CREATE THE BIRD
	STY	PDIST,U		A BIRD IS NOW AFTER THE MAN (BIRDS WORKSPACE)
	STU	PDIST,Y		 & EVEN GIVE THE BIRD TO THE MAN
	LDA	PEGG,U		MAINTAIN NBR OF EGGS LEFT IN THE BIRD
	STA	PEGG,Y
	CLR	PCHASE,Y	NOT CHASING THE PLAYER
	CLR	PFACE,Y		ASSUME BUZZARD COMING IN FROM LEFT HAND SIDE
	CLR	PRDIR,Y		ASSUME BIRD NEVER CAME WITHIN SHORT RANGE SCAN
	LDA	#8
	STA	PVELX,Y		AT MAXIMUM WARP SPEED
	LDD	#0
	STD	PVELY,Y		NOT FALLING YET
	STD	PRIDER,Y	& WITHOUT A RIDER
	CLR	PBUMPX,Y
	CLR	PTIMX,Y
	CLR	PFRAME,Y
	LDD	BUZARD_SHRAM
	STD	PHORSE,Y	GIVE BUZARD IMAGE
	LDD	PDECSN,U	DEFAULT INTELLIGENCE
	STD	PDECSN,Y
	LDD	#SEEKE		TELL THE DOGIE TO FETCH THE LITTLE MAN
	STD	PJOY,Y
	LDD	PPOSX,U		WHERE IS THE LITTLE MAN?
	SUBD	#(ERIGHT-ELEFT)/2
	BHI	EGGMRT		 BR=MAN ON RIGHT SIDE OF SCREEN
	COM	PFACE,Y		OPPS... COMING FROM RIGHT TO LEFT
	NEG	PVELX,Y
	LDD	#ERIGHT-1
	BRA	EGGMAN
*
EGGMRT	LDD	#ELEFT+1
EGGMAN	STD	PPOSX,Y
	LDA	PPOSY+1,U	FIND LEVEL TO START BIRD
	LDB	#$93+20+2	ASSUME STANDING ON CLIFF5
	CMPA	#($D3+$A3)/2	ON LEVEL5?
	BHI	L001_033		 BR=NO ON THIS LEVEL
	LDB	#$52+20+2	ASSUME STANDING ON CLIF4,CLIF3U,CLIF3R,CLIF3L
	CMPA	#($51+$81)/2	ON LEVEL4, LEVEL3, LEVEL2?
	BHI	L001_033		 BR=NO ON THIS LEVEL
	LDB	#$45		ASSUME ON LEVEL0, LEVEL1 (CLIF1R,CLIF1L,CLIF2)
L001_033:	CLRA
	STD	PPOSY,Y
	LDX	#EGGTBL		GET SIDE SHOW FOR THE EGG, WHILE THE BIRD IS IN
	STX	PRDIR,U		 FLIGHT (PRDIR & PPVELX)
	BRA	EGGNXF		START THE SHOW!!
*
EGGHCH	LEAX	3,X		NEXT EGG HATCHING FRAME
	STX	PRDIR,U
	JSR	VNAPTPC		NAP REG.A TIME
EGGNXF	JSR	CEGG
	JSR	VWR1CLS
	LDA	[PRDIR,U]		GET NEXT FRAME OFFSET
	JSR	WEGPAR
	LDX	PRDIR,U		GET SHOW TIME POINTER
	LDA	PPOSY+1,U	MAINTAIN COLISION DETECT POINTERS
	STA	PCOLY1,U
	SUBA	1,X
	STA	PCOLY2,U
	LDA	2,X
	BNE	EGGHCH		BR=MORE HATCHING FRAMES TO GO
*
	PCNAP	7
	JSR	CEGG		WRITE PROPER ENEMIES COLOR
	LDY	PDECSN,U
	LDY	[DPLYR,Y]
	LEAY	24,Y		POINT TO STANDING PLAYER
	STY	PPICH,U
	JSR	WRREGY
EGGLLP	PCNAP	8		WAIT UNTILL BUZZARD COMES OR KILLED BY PLAYER
	LDY	PPICH,U
	JSR	WRREGY
	BRA	EGGLLP
*
EGGBKG	LDX	PPOSX,U		REFRESH BACKGROUND FROM THE EGG
	LDA	BCKXTB,X	RE-PLOT BACKGROUND
	LDX	PPOSY,U
	ANDA	BCKYTB+5+6,X
	ORA	BCKRFS
	STA	BCKRFS
	RTS
*
EGGHIT
	JSR	CEGG
	JSR	EGGBKG		REFRESH BACKGROUND
	LDA	GOVER		ATTRACT MODE EGG SCORING?
	BLE	L010_021		 BR=NO
	JSR	ATTEGG		ATTRACT MODES CYCLING EGG
L010_021:	PCNAP	2
	JSR	EGAREA		CALC A ACCEPTABLE VISABLE SCREEN ADDRESS
	LDA	#1
	STA	PLAVT,U		INITIAL DELAY = NO TIME
	LDX	[PDUMMY]	GET 2ND PROCESS IN THE LIST
L020_019:	LDX	PLINK,X		NEXT PROCESS
	BEQ	L100_001		 BR=END SEARCH FOR OVERLAPPING SCORES
	LDA	PID,X		AN EGG SCORE?
	CMPA	#SCRID
	BNE	L020_019
	LDD	PPOSY,U		WITHIN 6 PIXELS IN Y DIRECTION?
	SUBD	PPOSY,X
	BPL	L031_001
	COMA
	NEGB
	SBCA	#-1
L031_001:	SUBD	#6
	BGE	L020_019		 BR=NO, NOT EVEN CLOSE
	LDD	PPOSX,U		WITHIN 16 PIXELS IN X DIRECTION?
	SUBD	PPOSX,X
	BPL	L032_000
	COMA
	NEGB
	SBCA	#-1
L032_000:	SUBD	#16
	BGE	L020_019		 BR=NO, NOT EVEN CLOSE
L033_000:	LDA	PJOYT,X		A FASTTER EGG  DISPLAY TIME?
	CMPA	#16/2
	BLO	L034_000		 BR=NO ONLY A FEW TICKS LEFT
	LDA	#16/2		YES, SPEED UP THE DISPLAYING SCORE TIME
	STA	PJOYT,X
	LDY	PLINK,X
	LDB	PID,Y		CAUGHT EGG I.D.?
	CMPB	#CATID
	BNE	L034_000
	STA	PJOYT,Y		YES, THIS TIME IS ALSO REDUCED
L034_000:	ASLA
	ADDA	PLAVT,X		ADD IN DELAY FACTOR
	CMPA	PLAVT,U		IS THIS TOTAL TIME LARGER?
	BLO	L020_019		 BR=NO, CURRENT DELAY IS THE LONGEST
	STA	PLAVT,U
	BRA	L020_019
*
L100_001:	LDA	#SCRID		EGG SCORES I.D.
	STA	PID,U
	LDA	#60/2		1 SECOUND DISPLAY COUNT
	STA	PJOYT,U
L101_000:	PCNAP	1
	DEC	PLAVT,U
	BNE	L101_000
	LDA	PEGG,U		DISPLAY THE SCORE IN THE AIR?
	BEQ	L111_000		 BR=NO
	LDX	#L112_000		DISPLAY THE SCORE IN THE AIR
	LDD	#CATID*256+$FF
	JSR	VCUPROC
	LDD	#MSG500*256+EGGCOL*$11	GET SCORE VALUE & COLOR
	STD	PRDIR,Y
	LDD	PPOSX,U
	STD	PPOSX,Y
	LDD	PPOSY,U
	SUBB	#6
	STD	PPOSY,Y
	LDD	PSTATE,U		MESSAGE LOCATION
	SUBB	#6
	STD	PSTATE,Y
	LDA	PJOYT,U		CURRENT DISPLAY TIME
	STA	PJOYT,Y
	CLR	PLAVT,Y		WITH NO DELAYS
L111_000:	PCNAP	2
L112_000:	BRA	EGGSC2
*
*	PUT UP SCORE VALUE IN THE AIR
*	 INPUT: PPOSX - X PIXEL POSITION
*		PPOSY - Y PIXEL POSITION
*		PRDIR,PPVELX - MESSAGE NBR & COLOR
*
SCRAIR	LDA	#60/2		1 SECOUND COUNT
	STA	PJOYT,U
	BSR	EGAREA		CALC A ACCEPTABLE VISABLE SCREEN ADDRESS
EGGSC2	LDX	PSTATE,U	GET AREA ON SCREEN TO PUT MESSAGE
	LDD	PRDIR,U		GET PRDIR (MESSAGE NUMBER) & PPVELX (COLOR)
	DEC	PJOYT,U
	BEQ	EGGSC3
	JSR	OUTP35
	PCNAP	2
	BRA	EGGSC2
*
EGGSC3	CLRB			ERASE SCORE
	JSR	OUTP35
	JSR	EGGBKG		RE-PLOT BACKGROUND
	JMP	VSUCIDE		end the process
*
EGGDON
	JSR	CEGG
EGGDN2	JMP	VSUCIDE		end the process
*
*	CALCULATE VISABLE EGG SCREEN AREA
*
EGAREA	LDD	PPOSX,U		CALC SCREEN ADDRESS
	ANDB	#$FE		UNFLAVOR AREA TO PUT SCORE
	STB	PPOSX+1,U
	RORA
	RORB
	TFR	B,A
	LDB	PPOSY+1,U
	SUBB	#5		RAISE THE SCORE (BASED ON TOP LEFT CORNER)
	CMPB	#$D3-1-4	MINIMUM LOWEST POINT IS TOP OF CLIFF5
	BLS	EGGPYM		 BR=OK AREA
	LDB	#$D3-1-4
EGGPYM	CMPA	#(288-16)/2		MESSAGE WILL NOT WRAP AROUND
	BLS	EGGPXM
	LDA	#(288-16)/2
EGGPXM	STD	PSTATE,U
	STB	PPOSY+1,U	CORRECTED AREA FOR DISPLAYING SCORE
	TFR	A,B
	CLRA
	ASLB
	ROLA
	STD	PPOSX,U
	RTS
*
*	ATTRACT MODES CYCLING EGG SCORE
*
ATTEGG	LDX	#L020_020		ATTRACT MODE EGG SCORE CYCLE
	LDD	#EGGID*256+$FF
	JSR	VCUPROC
	LDD	PPOSX,U
	STD	PPOSX,Y
	LDD	PPOSY,U
	STD	PPOSY,Y
	LDD	PRDIR,U
	STD	PRDIR,Y
	RTS
*
L010_022:	BSR	ATTEGG
	JMP	SCRAIR
L020_020:	PCNAP	60+8
	INC	PRDIR,U		NEXT MESSAGE NUMBER
	LDA	PRDIR,U
	CMPA	#4
	BLS	L010_022
	JMP	VSUCIDE		end the process
*
WEGG	JSR	VWR1CLS
	LDA	PVELX,U
	BPL	WEGRIT
	LDY	#EGFLFT		EGG FALLING TO THE LEFT
	BRA	WEGY
*
WEGRIT	LDY	#EGFRIT		ASSUME EGG FALLING TO THE RIGHT
WEGY	LDD	PVELY,U		HOW FAST IS EGG FALLING?
	BMI	WEGVM		 BR=EGG FALLING UP???
	SUBD	#$0080
	BGT	WEGD2		BR=NOT THIS SLOW
WEGUP	LDA	,Y		GET FRAME LEVEL FRAME
	BRA	WEGPAR
*
WEGD2	LDA	2,Y
	BRA	WEGPAR
*
WEGVM	ADDD	#$0080
	BGT	WEGUP
WEGD3	LDA	1,Y
WEGPAR	LDY	EGGI_SHRAM
	LEAY	A,Y
	STY	PPICH,U
	JSR	WRHOR2
	JMP	CLIPER		CLIP THE EGG AS NEEDED
*
*	EGG ANIMATION TABLE
*
EGFLFT	FCB	0,12,6		FALLING LEFT
EGFRIT	FCB	0,6,12		FALLING RIGHT
EGGTBL	FCB	6,6,7		WIGGLE LEFT
	FCB	0,6,3		WIGGLE UP
	FCB	12,6,7		WIGGLE RIGHT
	FCB	0,6,7+60	WIGGLE UP & PAUSE
	FCB	18,6,7		HATCH 1
	FCB	24,11,7		HATCH 2
	FCB	30,11,7		HATCH 3
	FCB	36,11,0		HATCH 4
*
*	BUZARD SEEKING MAN
*	 PDIST IS THE MANS WORKSPACE FOR PPOSX & PPOSY & HANDSHAKING
*
SEEKE	LDY	PDIST,U		GET THE MANS WORKSPACE
	CLRB
	LDA	PPOSY+1,Y	HOW ABOUT THE Y LEVEL
	CMPA	PPOSY+1,U
	BEQ	SEEKF1
	BLO	SEEKBL		 BR=BELOW TARGETED MAN
	LDB	PVELY,U		NOT TOO FAST A DROP!
	CMPB	#2
	BGE	SEEKBL
	LDX	PPOSX,U		IS A CLIFF IN THE WAY??
	LDA	BCKXTB+18,X	ASSUME FLYING TO THE RIGHT
	LDB	PVELX,U
	BPL	SEEKC2		 BR=ASSUMPTION CORRECT
	LDA	BCKXTB-18,X	LOOK IN OPPOSITE DIRECTION
SEEKC2	LDX	PPOSY,U
	CLRB			ASSUME NO FLAP
	ANDA	BCKYTB,X
	ANDA	#$08		CLIF3U IS A BITCH!
	BEQ	SEEKF1		 BR=NO CLIFF IN THE WAY
SEEKBL	CLRB
	LDA	PVELY,U
	CMPA	#-1		MAXIMUM UPWARD VELOCITY (MINIMIZE OVERSHOOT)
	BLT	SEEKF1
	LDD	#SEEKF2		FLAP ROUTINE
	STD	PJOY,U
	LDA	#4
	STA	PJOYT,U
	LDB	#1		FLAP WINGS, YOU ARE BELOW THE MAN
SEEKF1	PSHS	B
	LDD	PPOSX,Y		WHICH DIRECTION TO FACE?
	ADDD	#-12
	SUBD	PPOSX,U
	BPL	SEEKFR		 BR=LONG RANGE REG.X
	ADDD	#8+12
	BMI	SEEKFL		 BR=LONG RANGE REG.X
SEEKFS	LDA	#1
	STA	PRDIR,U		THE BIRD CAME WITHIN SHORT RANGE SENSORS
	LDX	PSTATE,U	MAKE SURE BIRD IS NOT TRAPPED ON ANOTHER CLIFF
	BEQ	SEEKAR		 BR=SEEKING FROM THE AIR
	LDA	PPOSY+1,Y
	CMPB	PPOSY+1,U
	BHI	SEEKFJ		BR=ON HIGHER CLIFF
	LDA	PVELX,U		FINALLY STOPPED??
	BEQ	MOUNTM		GOT THE MAN!!!
	LDA	PVELX,U		SLOW DOWN!!!
	BMI	SEEKR
	BRA	SEEKL
*
SEEKAR	LDA	PVELX,U		SLOW DOWN, BUT DO NOT REVERSE
	BMI	SEEKL3
	BGT	SEEKR3
	LDA	PFACE,U		SPEED =0, USE DIRECTION FACING FOR DIRECTION
	BMI	SEEKL
	BRA	SEEKR
*
SEEKL3	CMPA	#-2
	BEQ	SEEKF0		BR=SLOWEST SPEED
	BRA	SEEKR		HERE IF GOING TO THE LEFT TOO FAST
*
SEEKR3	CMPA	#2
	BEQ	SEEKF0		BR=SLOWEST SPEED
	BRA	SEEKL		HERE IF GOING TO THE RIGHT TOO FAST
*
SEEKFJ	LDA	PVELX,U		WRONG CLIFF TRY AGAIN
	BMI	SEEKL2
	BRA	SEEKR2
*
SEEKFL	LDA	PRDIR,U		DID BIRD EVER COM IN SHORT RANGE AREA?
	BNE	SEEKS
SEEKL2	LDA	PVELX,U
	BMI	SEEKF0
SEEKL	LDA	#-1
	PULS	B
	STD	CURJOY
	RTS
*
SEEKFR	LDA	PRDIR,U		DID BIRD EVER COM IN SHORT RANGE AREA?
	BNE	SEEKS
SEEKR2	LDA	PVELX,U
	BGT	SEEKF0
SEEKR	LDA	#1
	PULS	B
	STD	CURJOY
	RTS
*
SEEKF0	CLRA
	PULS	B
	STD	CURJOY
	RTS
*
SEEKS	LDD	PPOSX,Y
	SUBD	PPOSX,U		WHICH WAY IS THE SHORTEST WAY TO TRAVEL?
	BMI	SEEKSM
SEEKSP	CMPD	#(ERIGHT-ELEFT)/2	CHECK FOR WRAP AROUND
	BLT	SEEKSD
	SUBD	#ERIGHT-ELEFT		UNRAP THE DIFFERENCE
	BRA	SEEKSD
*
SEEKSM	CMPD	#-(ERIGHT-ELEFT)/2
	BGT	SEEKSD
	ADDD	#ERIGHT-ELEFT
SEEKSD	TSTA
	BMI	SEEKL2
	BRA	SEEKR2
*
MOUNTM	PULS	B		RESTORE STACK
	LDD	#MOUNLP
	STD	PJOY,U
	LDA	#5+1		WAIT A BIT
	STA	PJOYT,U
MOUNLP	DEC	PJOYT,U
	BEQ	MOUNRI		BR=TIME FOR RIDER
	LDA	#1		RUN TO THE RIGHT FOR YOUR LIFE
	LDB	PFACE,U
	BPL	MOUNRN
	NEGA
MOUNRN	CLRB
	STD	CURJOY
	RTS
*
MOUNRI	INC	NSMART		JUST GOT SMARTER
	INC	PCHASE,U
	LDX	#SNMOUN		MAKE A MOUNT THE BUZZARD SOUND
	JSR	VSND
	LDX	PDIST,U		KILL EGG ROUTINE
	LDD	#EGGDON
	STD	PPC,X
	LDA	#1		WAKE-UP IMMEDIATELY
	STA	PNAP,X
	LDA	PID,X		NO-LONGER COLISIONABLE
	ANDA	#$7F
	STA	PID,X
	LDX	PDECSN,U
	LDA	GOVER		GAME OVER?
	BNE	L002_017		 BR=NO
	LDD	DJOY,X		YES, SELECT A DUMMIE DUM DUM BIRD
	BRA	L003_009
*
L002_017:	LDD	DSMART,X	SELECT PROPER JOYSTICK ROUTINE
L003_009:	STD	PJOY,U
	JMP	PLYLI2		ENEMY RE-INCARNATED!!! (WHO CAES ABOUT STACK)
*
SEEKF2	DEC	PJOYT,U		FLAP ROUTINE
	BEQ	SEEKF3
	LDD	#1
	STD	CURJOY
	RTS
*
SEEKF3	LDD	#SEEKE
	STD	PJOY,U
	LDD	#0
	STD	CURJOY
	RTS
*
*	RADDR
*
RADDR	JSR	VRAND
	ROLA
	LDB	#$10
	MUL
	ORA	#$A0
	TFR	D,X
	INC	,X
	RTS
*
*	LINE TRACKING INTELLIGENCE
*
LINET	LDA	NSMART		GET NUMBER OF SMART ENEMIES
	CMPA	WSMART		BELOW MINIMUM INTELLIGENCE?
	BLO	LNTSMT		 BR=YES, GET SMARTER
	DEC	PLAVT,U
	BGT	L001_038
	LDA	LNTLAV		LINE TRACKING LAVA TROLL LOOKER
	STA	PLAVT,U
	LDX	PPREV		LAVA TROLL AFTER ME?
	LDA	PID,X
	CMPA	#LAVID
	BEQ	LNTUP
L001_038:	LDA	#AOFFL1		ASSUME NEAR TRACKING LINE 1
	LDB	PPOSY+1,U
	CMPB	#AOFFL1+AOFFUD
	BLO	LNTTRK		 BR=TRACK ON LINE 1
	LDA	#AOFFL2		ASSUME NEAR TRACKING LINE 2
	CMPB	#AOFFL2+AOFFUD
	BLO	LNTTRK		 BR=TRACK ON LINE 2
	LDA	#AOFFL3		ASSUME NEAR TRACKING LINE 3
LNTTRK	CLRB			ASSUME ON TRACKING LINE
	SUBA	PPOSY+1,U	ABOVE OR BELOW LINE?
	BPL	LNTFLP		BR=AROUND THE LINE
	LDA	PVELY,U		ALREADY GOING UP?
	BMI	LNTFLP		 BR=YES, SO WAIT TILL NEXT TIME
LNTUP	LDD	#LNTOFP		GET OFF GROUND OR JUST FLAP
	STD	PJOY,U
	LDB	#1
LNTFLP	TST	PFACE,U		MOVE IN DIRECTION OF FACING
	BMI	LNTLF2
	LDA	#1
	STD	CURJOY
	RTS
*
LNTLF2	LDA	#-1
	STD	CURJOY
	RTS
*
LNTOFP	LDD	#LINET
	STD	PJOY,U
	CLRB
	BRA	LNTFLP
*
LNTSMT	INC	NSMART		JUST GOT SMARTER
	INC	PCHASE,U
	LDX	PDECSN,U
	LDX	DSMART,X
	STX	PJOY,U		NEW SMARTS
	JMP	,X		START THE SMARTS
*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE
*
	FCC	'JOUST (C) 1982 WILLIAMS ELECTRONICS INC.'
*
*	BOUNDAR
*
DYLEN	EQU	$14-6
*
BOUNDR	DEC	PLAVT,U
	BGT	L001_040
	LDA	LNTLAV		LINE TRACKING LAVA TROLL LOOKER
	STA	PLAVT,U
	LDX	PPREV		LAVA TROLL AFTER ME?
	LDA	PID,X
	CMPA	#LAVID
	BEQ	BODN1A
L001_040:
	JSR	SELPLY		SELECT TARGETED PLAYER
	BEQ	BOLEVV		BR=NO PLAYERS HERE
	LDD	PPOSY,X
	SUBD	PPOSY,U
	LBLT	BOUNUP
BOUNDN	CMPD	BODNRG		#DYLEN		LONG OR SHORT RANGE SEEK
	BLT	BOLEVV		 BR=SHORT RANGE SEEK
BODN	LDD	BODNDI		-DYLEN*256	SET DISTANCE TO GO DOWN
	STD	PDIST,U
	LDD	#BODN1		FLAP WINGS
	STD	PJOY,U
	BRA	BODN10
*
BOLEVV	JMP	BOLEV
*
BODN1	LDD	PSTATE,U	ON THE GROUND?
	BNE	BOBRAIN		 BR=YES, CANNOT GO DOWN ANY FURTHER
	LDD	PVELY,U		GOING DOWN?
	BMI	BOUP1B		BODN1B	 BR=NO
	ADDD	PDIST,U
	BPL	BOBRAIN		BACK TO THE BRAINS
	STD	PDIST,U
BODN10	LDD	PVELY,U
BODN11	SUBD	BODNVY		#$0100		FALLING NOT TOO FAST?
	BMI	BOUP1B		BODN1B	 BR=NO
BODN1A	LDD	#BODN2		FLAP WINGS
	STD	PJOY,U
	LDA	#2
	BRA	BOUP12
********	STA	PJOYT,U		WING DOWN TIME
********	LDB	#$01
********	BRA	BODIRL
*
BODN2	LDD	PVELY,U
	BMI	BODN22
	ADDD	PDIST,U
	BPL	BOBRAIN		BACK TO THE BRAINS
	STD	PDIST,U
BODN22	LDB	#1		ASSUME WINGS DOWN
	DEC	PJOYT,U		WING DOWN TIME UP??
	BGT	BODN2A		 BR=NO
	LDD	#BODN1
	STD	PJOY,U
	CLRB			WINGS UP
BODN2A	BRA	BODIRL
*
BOBRAIN	JMP	BOUNDR
*
BOUNUP	CMPD	BOUPRG		#-DYLEN		LONG OR SHORT RANGE SEEK
	BGT	BOLEV		 BR=SHORT RANGE SEEK
BOUP	LDX	PPOSX,U		CLIFF IN THE WAY??
	LDY	PPOSY,U
	LDA	BCKXTB,X
	ANDA	BCKYTB-DYLEN,Y
	BNE	BOLEV		 BR=YES
	LDD	BOUPDI		#DYLEN*256	NO, SET DISTANCE TO GO UP
	STD	PDIST,U
	BRA	BOUP1A
*
BOUP1	LDD	PVELY,U		GOING UP?
	BPL	BOUP11		 BR=NO
	ADDD	PDIST,U
	BMI	BOBRAIN		BACK TO THE BRAINS
	STD	PDIST,U
BOUP11	DEC	PJOYT,U		TIME TO FLAP WINGS
	BGT	BOUP1B		 BR=NO
BOUP1A	LDD	#BOUP2		FLAP WINGS
	STD	PJOY,U
	LDA	BOUPWD		#2
BOUP12	STA	PJOYT,U		WING DOWN TIME
	LDB	#$01
	BRA	BODIRL
*
BOUP1B	CLRB			WINGS UP
BODIRL	LDA	PPOSY+1,U	BELOW CLIF5?
	CMPA	#$D3
	BLO	BODIR		 BR=NO
	LDA	PVELY,U		ALREADY GOING UP?
	LBPL	BOLAVA		 BR=NO, GO UP FAST BEFORE THE LAVA GETS ME!!
*
BODIR	LDA	PFACE,U
	BMI	BODN1C
	LDA	#1
	STD	CURJOY
	RTS
*
BODN1C	LDA	#-1
	STD	CURJOY
	RTS
*
BOUP2	LDD	PVELY,U
	BPL	BOUP22
	ADDD	PDIST,U
	BMI	BOBRAIN		BACK TO THE BRAINS
	STD	PDIST,U
BOUP22	LDB	#1		ASSSUME WINGS DOWN
	DEC	PJOYT,U		WING DOWN TIME UP??
	BGT	BOUP2A		 BR=NO
	LDA	BOUPWU		#8
	STA	PJOYT,U
	LDD	#BOUP1
	STD	PJOY,U
	CLRB			WINGS UP
BOUP2A	BRA	BODIRL
*
BOBRA2	JMP	BOUNDR	BACK TO THE BRAINS
*
BOLEV	LDD	#BOLEV1		LEVEL FLIGHT
	STD	PJOY,U
	LDD	PPOSY,U		REMEMBER LINE TO TRACK
	STD	PDIST,U
	LDA	PVELX,X		& PLAYERS VELOCITY
	STA	PPVELX,U
	LDA	BOLETM		#20+1		TIME UNTIL NEXT DECISION
	STA	PJOYT,U
BOLEV1	DEC	PJOYT,U
	BLE	BOBRA2
	LDB	PPOSY+1,U
	CMPB	#$D3		BELOW CLIF5?
	BLO	L003_010		 BR=NO
	JSR	SELPLY		SELECT TARGETED PLAYER
	BEQ	BOFAST		BR=NO PLAYERS HERE, GO UP
	LDD	PPOSX,X		IS PLAYER CLOSE BY?
	ADDD	#3*18+9
	CMPD	PPOSX,U
	BLT	BOFAST		BR=NO, GO UP
	ADDD	#-2*(3*18+9)
	CMPD	PPOSX,U
	BGT	BOFAST		BR=GO UP FAST BEFORE THE LAVA GETS ME!!
*				IF HERE, LET HIM LOURE ME INTO LAVA
L003_010:	LDA	PVELY,U		FALLING?
	BMI	BOLEVA		 BR=NO
	LDB	PDIST+1,U	ABOVE TRACKING LINE
	CMPB	PPOSY+1,U
	BLO	BOLEVA		 BR=YES
BOFAST	LDD	#BOLEV2		FAST EXIT UP
	STD	PJOY,U
	LDB	#$01		FLAP WINGS
	BRA	BOLEVB
*
BOLEV2	LDD	#BOLEV1
	STD	PJOY,U
BOLEVA	CLRB			WINGS UP
BOLEVB	LDA	PPVELX,U	DO NOT COPY PLAYERS MOVES TOO OFTEN
	CMPA	PVELX,U
	BNE	BODIR3
	DEC	PRDIR,U
	BMI	BODIR3
	CLR	PRDIR,U
	COM	PFACE,U		TRY THE OTHER DIRECTION
BODIR3	JMP	BODIR
*
BOLAV1	LDA	PPOSY+1,U
	CMPA	#$D3		BELOW CLIF5?
	BLO	BOLAV4		 BR=NOT ANY MORE
	LDA	PVELY,U		YES, GOING UP?
	BMI	BOLAV4		 BR=YES, IGNORE LAVA
BOLAVA	LDD	#BOLAV2
	STD	PJOY,U
	LDB	#1
	BRA	BODIR3
*
BOLAV2	LDD	#BOLAV1
	STD	PJOY,U
	CLRB
	BRA	BODIR3
*
BOLAV4	LDX	PDECSN,U	BACK TO THE BRAINS
	JMP	[DSMART,X]
*
*	ADVANCED BOUNDAR (HUNTER)
*
B2YLEN	EQU	$14-6
B2XLEN	EQU	27+4
*
B2UNDR	DEC	PLAVT,U
	BGT	L001_041
	LDA	LNTLAV		LINE TRACKING LAVA TROLL LOOKER
	STA	PLAVT,U
	LDX	PPREV		LAVA TROLL AFTER ME?
	LDA	PID,X
	CMPA	#LAVID
	BEQ	B2DN1A
L001_041:	JSR	SELPLY		SELECT TARGETED PLAYER
	BEQ	B2LEVV		BR=NO PLAYERS HERE
	LDD	PPOSY,X
	SUBD	PPOSY,U
	LBLT	B2UNUP
B2UNDN	SUBD	HUDNRG		#B2YLEN		LONG OR SHORT RANGE SEEK
	BLT	B2LEVV		 BR=SHORT RANGE SEEK
*
B2DN
B2DNST	LDD	HUDNDI		#-B2YLEN*256	NO, SET DISTANCE TO GO DOWN
	STD	PDIST,U
	LDD	#B2DN2		FLAP WINGS
	STD	PJOY,U
	BRA	B2DN1B
*
B2LEVV	JMP	B2LEV
*
B2DN1	LDD	PSTATE,U	ON THE GROUND?
	BNE	B2BRAIN		 BR=YES
	LDD	PVELY,U		GOING DOWN?
	BMI	B2DN1B		 BR=NO
	ADDD	PDIST,U
	BPL	B2BRAIN		BACK TO THE BRAINS
	STD	PDIST,U
	LDD	PVELY,U
B2DN11	SUBD	HUDNVY		#$0200		FALLING NOT TOO FAST?
	BMI	B2DN1B		 BR=NO
B2DN1A	LDD	#B2DN2		FLAP WINGS
	STD	PJOY,U
	LDA	#2
	STA	PJOYT,U		WING DOWN TIME
B2DN2B	LDB	#$01
	JMP	B2DIRL
*
B2DN2	LDD	PVELY,U
	BMI	B2DN22
	ADDD	PDIST,U
	BPL	B2BRAIN		BACK TO THE BRAINS
	STD	PDIST,U
B2DN22	LDB	#1		ASSUME WINGS DOWN
	DEC	PJOYT,U		WING DOWN TIME UP??
	BGT	B2DIR2		 BR=NO
	LDD	#B2DN1
	STD	PJOY,U
B2DN1B	CLRB			WINGS UP
B2DIR2	JMP	B2DIRL
*
B2BRAIN	JMP	B2UNDR
*
B2UNUP	CMPD	HUUPRG		#-B2YLEN		LONG/SHORT RANGE SEEK
	BGT	B2LEVV		 BR=SHORT RANGE SEEK
B2UP	LDX	PPOSX,U		CLIFF IN THE WAY??
	LDY	PPOSY,U
	LDA	BCKXTB,X
	ANDA	BCKYTB-B2YLEN,Y
	LBNE	B2UP3		 BR=YES
B2UPST	LDD	HUUPDI		#B2YLEN*256	NO, SET DISTANCE TO GO UP
	STD	PDIST,U
	BRA	B2UP2D
*
B2UP2	LDD	PVELY,U
	BPL	B2UP22
	ADDD	PDIST,U
	BMI	B2BRAIN		BACK TO THE BRAINS
	STD	PDIST,U
B2UP22	LDB	#1		ASSSUME WINGS DOWN
	DEC	PJOYT,U		WING DOWN TIME UP??
	BGT	B2UP2A		 BR=NO
B2UP2D	LDA	HUUPWU		#8
	STA	PJOYT,U
	LDD	#B2UP1
	STD	PJOY,U
	CLRB			WINGS UP
B2UP2A	BRA	B2DIRL
*
B2LEV	LDD	#B2LEV1		LEVEL FLIGHT
	STD	PJOY,U
	LDD	PPOSY,U		REMEMBER LINE TO TRACK
	STD	PDIST,U
	LDA	PVELX,X		& PLAYERS VELOCITY
	STA	PPVELX,U
	LDA	HULETM		#20+1		TIME UNTIL NEXT DECISION
	STA	PJOYT,U
*
B2LEV1	DEC	PJOYT,U
	LBLE	B2BRA2
	LDA	PVELY,U		FALLING?
	LBMI	B2LEVA		 BR=NO
	LDB	PPOSY+1,U	YES, BELOW CLIF5?
	CMPB	#$D3
	BLO	L003_011		 BR=NO
	JSR	SELPLY		SELECT TARGETED PLAYER
	BEQ	B2FAST		BR=NO PLAYERS HERE, GO UP
	LDD	PPOSX,X		IS PLAYER CLOSE BY?
	ADDD	#2*18+9
	CMPD	PPOSX,U
	BLT	B2FAST		BR=NO, GO UP
	ADDD	#-2*(2*18+9)
	CMPD	PPOSX,U
	BGT	B2FAST		BR=GO UP FAST BEFORE THE LAVA GETS ME!!
*				IF HERE, LET HIM LOURE ME INTO LAVA
L003_011:	LDB	PDIST+1,U	AB2VE TRACKING LINE
	CMPB	PPOSY+1,U
	LBLO	B2LEVA		 BR=YES
B2FAST	LDD	#B2LEV2
	STD	PJOY,U
	LDB	#$01		FLAP WINGS
*
B2LE11	LDA	PPVELX,U	DO NOT COPY PLAYERS MOVES TOO OFTEN
	CMPA	PVELX,U
	BNE	B2DIR
	DEC	PRDIR,U
	BMI	B2DIR
	CLR	PRDIR,U
	COM	PFACE,U		TRY THE OTHER DIRECTION
	BRA	B2DIR
*
*
B2DIRL	LDA	PPOSY+1,U	BELOW CLIF5?
	CMPA	#$D3
	BLO	B2DIR
	LDA	PVELY,U		GOING DOWN?
	BMI	B2DIR		 BR=NO
	JMP	BOLAVA		STANDARD WAY TO AVOID THE LAVA
*
B2DIR	LDA	PVELX,U		WHICH DIRECTION GOING?
	BEQ	B2DIRA
	BPL	B2DILR
	PSHS	B
	LDD	PVELY,U		SEEKING DESTINATION CLIFF
	ASLB
	ROLA
	ASLB
	ROLA
	ASLB
	ROLA
	PULS	B
	LDY	PPOSY,U
	LDX	PPOSX,U
	LEAY	A,Y
	LDA	BCKXTB-B2XLEN,X
	ANDA	BCKYTB,Y
	BEQ	B2DIRA		 BR=YES
	CLR	PFACE,U		FACE RIGHT
	BRA	B2DICL
*
B2DILR	PSHS	B
	LDD	PVELY,U
	ASLB
	ROLA
	ASLB
	ROLA
	ASLB
	ROLA
	PULS	B
	LDY	PPOSY,U
	LDX	PPOSX,U
	LEAY	A,Y
	LDA	BCKXTB+B2XLEN,X
	ANDA	BCKYTB,Y
	BEQ	B2DIRA		 BR=YES
	LDA	#-1
	STA	PFACE,U		FACE LEFT
B2DICL	LDD	#B2AV		SLOW DOWN!!! GOING INTO A CLIFF
	STD	PJOY,U
	LDA	#8
	STA	PJOYT,U
	LDB	#1
*
B2DIRA	LDA	PBUMPX,U	FACE, BUMPED DIRECTION
	BEQ	B2FDIR
	STA	PFACE,U
B2FDIR	LDA	PFACE,U
	BMI	B2DN1C
	LDA	#1
	STD	CURJOY
	RTS
*
B2DN1C	LDA	#-1
	STD	CURJOY
	RTS
*
*
B2LEV2	LDD	#B2LEV1
	STD	PJOY,U
B2LEVA	CLRB			WINGS UP
	JMP	B2LE11
*
B2BRA2	JMP	B2UNDR	BACK TO THE BRAINS
*
B2UP1	LDD	PVELY,U		GOING DOWN?
	BPL	B2UP11		 BR=NO
	ADDD	PDIST,U
	BMI	B2BRA2		BACK TO THE BRAINS
	STD	PDIST,U
B2UP11	DEC	PJOYT,U		TIME TO FLAP WINGS
	BGT	B2UP1B		 BR=NO
	INC	PJOYT,U
	LDD	PVELY,U
	CMPD	HUUPVY		#-$0100
	BLT	B2UP1B
B2UP1A	LDD	#B2UP2		FLAP WINGS
	STD	PJOY,U
	LDA	HUUPWD		#2
	STA	PJOYT,U		WING DOWN TIME
	LDB	#$01
	JMP	B2DIRL
*
B2UP1B	CLRB			WINGS UP
	JMP	B2DIRL
*
B2AV	CLRB
	DEC	PJOYT,U
	BGT	B2DIRA
	JMP	B2UNDR
*
B2UP3	LDD	#B2UP3A		LEVEL FLIGHT, READY TO GO UP
	STD	PJOY,U
	LDD	PPOSY,U		REMEMBER LINE TO TRACK
	STD	PDIST,U
	LDA	#20+1		TIME UNTIL NEXT DECISION
	STA	PJOYT,U
*
B2UP3A	LDX	PPOSX,U		CLIFF IN THE WAY??
	LDY	PPOSY,U
	LDA	BCKXTB,X
	ANDA	BCKYTB-B2YLEN,Y
	LBEQ	B2UPST
	DEC	PJOYT,U
	LBLE	B2UNDR
	LDD	PVELY,U		FALLING FAST ENOUGH?
	ADDD	#-$0040
	BMI	B2UP3B		 BR=NO
	LDB	PDIST+1,U	ABOVE TRACKING LINE
	CMPB	PPOSY+1,U
	BLO	B2UP3B		 BR=YES
	LDD	#B2UP4
	STD	PJOY,U
	LDB	#$01		FLAP WINGS
	JMP	B2DIRA
*
B2UP4	LDD	#B2UP3A
	STD	PJOY,U
B2UP3B	CLRB			FLAP WINGS
	JMP	B2DIRA
*
*	SHADOW LORD
*
SHYLEN	EQU	$14-6
SHXLEN	EQU	27+4
*
SHADOW	DEC	PLAVT,U
	BGT	L001_042
	LDA	LNTLAV		LINE TRACKING LAVA TROLL LOOKER
	STA	PLAVT,U
	LDX	PPREV		LAVA TROLL AFTER ME?
	LDA	PID,X
	CMPA	#LAVID
	BEQ	SHUPST
L001_042:	JSR	SELPLY		SELECT TARGETED PLAYER
	LBEQ	SHLEV		BR=NO PLAYERS HERE
	LDD	PPOSY,X
	SUBD	PPOSY,U
	LBLT	SHUNUP
SHUNDN	SUBD	SHDNRG		#SHYLEN		LONG OR SHORT RANGE SEEK
	BLT	SHLEP		 BR=SHORT RANGE SEEK
*
SHDN	LDD	#SHDN2		NO FLAPING WINGS
	STD	PJOY,U
	CLRB			WINGS UP
	LDA	PPOSY+1,U	BELOW CLIF5?
	CMPA	#$D3
	BLO	L003_012		 BR=NO
	LDA	PVELX,U		FALLING?
	BMI	L003_012		 BR=NO
	INCB			FLAP YOUR WINGS!!!, HOT LAVA BELOW
L003_012:	JMP	SHDIRB
*
SHUNUP	CMPD	SHUPRG		#-SHYLEN	LONG OR SHORT RANGE SEEK
	BGT	SHLEP		 BR=SHORTEST RANGE SEEK
	LDX	PPOSX,U		CLIFF IN THE WAY??
	LDY	PPOSY,U
	LDA	BCKXTB,X
	ANDA	BCKYTB-SHYLEN,Y
	LBNE	SHUP3		 BR=YES
SHUPST	LDD	#SHUP1
	STD	PJOY,U
SHUP0	CLRB			WINGS UP
	JMP	SHDIRB
*
SHUP1	LDD	#SHADOW		FLAP WINGS
	STD	PJOY,U
	LDD	PVELY,U
	CMPD	SHUPVY		#-$0200
	BLT	SHUP0
	LDB	#$01
	JMP	SHDIRB
*
SHLEP	LDD	#SHLEP1		LEVEL FLIGHT
	STD	PJOY,U
	LDB	PPOSY+1,X	PLAYERS LINE TO TRACK
	STB	PDIST+1,U
	LDA	PVELX,X		& PLAYERS VELOCITY
	STA	PPVELX,U
	LDA	SHUPTM		#8+1		TIME UNTIL NEXT DECISION
	STA	PJOYT,U
*
SHLEP1	DEC	PJOYT,U
	LBLE	SHBRA2
	LDB	PPOSY+1,U
	CMPB	#$D3		GETTING TOO CLOSE TO THE LAVA?
	BLO	L002_018		 BR=YES
	LDA	PVELY,U		ALREADY GOING UP?
	BPL	SHFAST		 BR=NO
L002_018:	CMPB	PDIST+1,U	ABOVE TRACKING LINE
	BLS	SHLEPA		 BR=YES
SHFAST	LDD	#SHLEP2
	STD	PJOY,U
	LDB	#$01		FLAP WINGS
	BRA	SHLEPB
*
SHLEP2	LDD	#SHLEP1
	STD	PJOY,U
SHLEPA	CLRB			WINGS UP
SHLEPB	LDA	PPVELX,U	DO NOT COPY PLAYERS MOVES TOO OFTEN
	CMPA	PVELX,U
	LBNE	SHDIRA
	DEC	PRDIR,U
	LBMI	SHDIRA
	CLR	PRDIR,U
	COM	PFACE,U		TRY THE OTHER DIRECTION
	JMP	SHDIRA
*
SHLEV	LDD	#SHLEV1		LEVEL FLIGHT
	STD	PJOY,U
	LDB	PPOSY+1,U	REMEMBER LINE TO TRACK
	STB	PDIST+1,U
	LDA	SHLETM		#8+1		TIME UNTIL NEXT DECISION
	STA	PJOYT,U
*
SHLEV1	DEC	PJOYT,U
	LBLE	SHBRA2
	LDA	PVELY,U		FALLING?
	LBMI	SHLEVA		 BR=NO
	LDB	PDIST+1,U	ABOVE TRACKING LINE
	CMPB	PPOSY+1,U
	LBLO	SHLEVA		 BR=YES
	LDD	#SHLEV2
	STD	PJOY,U
	LDB	#$01		FLAP WINGS
*
SHDIR	LDA	PPOSY+1,U	BELOW CLIF5?
	CMPA	#$D0
	BLO	L001_043		 BR=NO
	LDA	PVELY,U		FALLING DOWN?
	LBPL	BOLAVA		 BR=YES, AVOID THE LAVA
L001_043:	LDA	PVELX,U		WHICH DIRECTION GOING?
	BEQ	SHDIRA
	BPL	SHDILR
	PSHS	B
	LDD	PVELY,U		SEEKING DESTINATION CLIFF
	ASLB
	ROLA
	ASLB
	ROLA
	ASLB
	ROLA
	PULS	B
	LDY	PPOSY,U
	LDX	PPOSX,U
	LEAY	A,Y
	LDA	BCKXTB-SHXLEN,X
	ANDA	BCKYTB,Y
	BEQ	SHDIRA		 BR=YES
	CLR	PFACE,U		FACE RIGHT
	BRA	SHDICL
*
SHDILR	PSHS	B
	LDD	PVELY,U
	ASLB
	ROLA
	ASLB
	ROLA
	ASLB
	ROLA
	PULS	B
	LDY	PPOSY,U
	LDX	PPOSX,U
	LEAY	A,Y
	LDA	BCKXTB+SHXLEN,X
	ANDA	BCKYTB,Y
	BEQ	SHDIRA		 BR=YES
	LDA	#-1
	STA	PFACE,U		FACE LEFT
SHDICL	LDD	#SHAV		SLOW DOWN!!! GOING INTO A CLIFF
	STD	PJOY,U
	LDA	SHCLTM		#8
	STA	PJOYT,U
	LDB	#1
*
SHDIRA	LDA	PBUMPX,U	FACE, BUMPED DIRECTION
	BEQ	SHFDIR
	STA	PFACE,U
SHFDIR	LDA	PFACE,U
	BMI	SHDN1C
	LDA	#1
	STD	CURJOY
	RTS
*
SHDIRB	LDA	PVELX,U
	BEQ	SHDIRA
	CLRA
	STD	CURJOY
	RTS
*
SHDN1C	LDA	#-1
	STD	CURJOY
	RTS
*
*
SHLEV2	LDD	#SHLEV1
	STD	PJOY,U
SHLEVA	CLRB			WINGS UP
	JMP	SHDIR
*
SHBRA2	JMP	SHADOW	BACK TO THE BRAINS
*
SHAV	CLRB
	DEC	PJOYT,U
	BGT	SHDIRA
	JMP	SHADOW
*
SHUP3	LDD	#SHUP3A		LEVEL FLIGHT, READY TO GO UP
	STD	PJOY,U
	LDB	PPOSY+1,U	REMEMBER LINE TO TRACK
	STB	PDIST+1,U
	LDA	#20+1		TIME UNTIL NEXT DECISION
	STA	PJOYT,U
*
SHUP3A	LDX	PPOSX,U		CLIFF IN THE WAY??
	LDY	PPOSY,U
	LDA	BCKXTB,X
	ANDA	BCKYTB-SHYLEN,Y
	LBEQ	SHUPST
	DEC	PJOYT,U
	LBLE	SHADOW
	LDD	PVELY,U		FALLING FAST ENOUGH?
	ADDD	#-$0040
	BMI	SHUP3B		 BR=NO
	LDB	PDIST+1,U	ABOVE TRACKING LINE
	CMPB	PPOSY+1,U
	BLO	SHUP3B		 BR=YES
	LDD	#SHUP4
	STD	PJOY,U
	LDB	#$01		FLAP WINGS
	JMP	SHDIRA
*
SHUP4	LDD	#SHUP3A
	STD	PJOY,U
SHUP3B	CLRB			FLAP WINGS
	JMP	SHDIRA
*
SHDN2	LDD	#SHADOW
	STD	PJOY,U
	CLRB
	LDA	PVELX,U
	LBEQ	SHDIRA
	LDA	PSTATE,U	ON THE GROUND?
	BEQ	L001_044		 BR=NO IN THE AIR
	LDX	PPOSX,U		CLIF3U IN THE WAY?
	LDA	BCKXTB,X
	LDX	PPOSY,U
	ANDA	BCKYTB,X
	BEQ	L001_044		 BR=NO
	LDB	#1		FLAP WINGS
L001_044:	CLRA
	STD	CURJOY
	RTS
*
*
*	SELECT CLOSEST PLAYER
*	 OUTPUT REG.X, TARGETED PLAYER (IF STATUS .NE. IS TRUE)
*
SELPLY	LDX	TARPLY		GET TARGETED PLAYER TO KILL
	BEQ	SPNONE
	LDA	TARTM1		DELAY TIMER AT ZERO?
	BEQ	L020_021		 BR=NO, TRY PLAYER 2
	LDX	TARPL2		PLAYER 2 HERE?
	BEQ	SPNONE		 BR=NO
	LDA	TARTM2		PLAYER 2'S DELAY TIMER AT ZERO?
	BEQ	SPN3PL		BR=GET PLAYER 2!
	BRA	SPNONE		BR=NO ONE TO CHASE
*
L020_021:	LDY	TARPL2		PLAYER #2
	BEQ	SPN3PL		 BR=ONLY 1 PLAYER NOW
	LDA	TARTM2
	BNE	SPN3PL
	LDB	PPOSY+1,X
	SUBB	PPOSY+1,U	FIND CLOSEST CO-ORDINANT
	BLO	SPRYX
	NEGB
SPRYX	STB	,-S
	LDD	PPOSX,X
	SUBD	PPOSX,U
	BLO	SPRXX
	COMA
	COMB
	ADDD	#-1
SPRXX	TSTA
	BNE	SPRLOX
	CMPB	,S
	BHI	SPRLOX
	STB	,S
SPRLOX	EQU	*
*
	LDB	PPOSY+1,Y
	SUBB	PPOSY+1,U	FIND CLOSEST CO-ORDINANT
	BLO	SPRYY
	NEGB
SPRYY	STB	,-S
	LDD	PPOSX,Y
	SUBD	PPOSX,U
	BLO	SPRXY
	COMA
	COMB
	ADDD	#-1
SPRXY	TSTA
	BNE	SPRLOY
	CMPB	,S
	BHI	SPRLOY
	STB	,S
SPRLOY	EQU	*
*
	CMPB	1,S
	PULS	D
	BLO	SPN3PL
SPN2PL	LDX	TARPL2
SPN3PL	ANDCC	#$FB			;#!N$04		CLEAR ZERO BIT FOR .NE. BEING TRUE
	RTS
*
SPNONE	ORCC	#$04		SETT ZERO BIT FOR .EQ. BEING TRUE
	RTS
*
*	KEN LANTZ SPECIAL EFFECTS, PLAYER POOF (DEATH)
*
* a sequence of three images lasting 1 sec total
DEATH	LDD	PPOSX,U
	CMPD	#4		LET PLAYER SEE THE EXPLOSION
	BGT	L002_019
	LDD	#4		NEW POSITION IN X DIRECTION
L002_019:	CMPD	#288
	BLT	L001_045
	LDD	#288
L001_045:	STD	PPOSX,U
	LDY	POOF1_SHRAM		POOF FRAME 1
	JSR	WRREGY
	PCNAP	5
	LDY	POOF1_SHRAM
	JSR	CLREGY
	LDY	POOF2_SHRAM		POOF FRAME 2
	JSR	WRREGY
	PCNAP	8
	LDY	POOF2_SHRAM
	JSR	CLREGY
	LDY	POOF3_SHRAM		POOF FRAME 3
	JSR	WRREGY
	PCNAP	12
	LDY	POOF3_SHRAM
	JSR	CLREGY
* test PPOSX/Y for nearest cliff and refresh if necessary
	LDX	PPOSX,U
	LDY	PPOSY,U
	LDA	BCKYTB-15,Y
	ORA	BCKYTB-13,Y
	ANDA	BCKXTB,X
	ORA	BCKRFS
	STA	BCKRFS
	JMP	VSUCIDE		end the process
*
*	PROCESS TO DESTROY A CLIFF
*		BY KEN LANTZ 6/10/82
*	INPUT PFRAME-2,U = CLIFF TO DESTROY
*
CLFDES
	LDA	#5
	STA	PFRAME,U	number of shakes to do
L001_046:	LDY	PFRAME-2,U
	JSR	BCKYUP
	PCNAP	10		!
	LDY	PFRAME-2,U	!
	JSR	BCKYUP		!
	LDA	#$2A		by altering the cliffs flavor
	STA	WCDMA,X
	PCNAP	10
	DEC	PFRAME,U
	BNE	L001_046
	JSR	LOCCLR		go erase the image as we will replace with
	PCNAP	2		debris
******************************************************************************
	LDY	#FIRSTI		point to the image area
	LDA	#5		where there are five images
	STA	PFRAME+2,U
L002_020:	LDX	PFRAME-2,U
	CMPX	$0000		here we do adjustments to center this is the RAM location that points to CLIF1L
	BNE	L021_002		and offset out debris images on the
	LDX	#$F145		larger and edge located cliffs
	BRA	L022_001
L021_002:	CMPX	$0004		$0000 and $0004 are CLIF1L and CLIF2 vectors,
	BNE	L023_001		respectively
	LDX	#$3151
	BRA	L022_001
L023_001:	LDX	4,X		Get destination
L022_001:	STX	PFRAME,U	Adjust destination for centering on cliffs
	BSR	CLIFER
	STY	PFRAME+3,U	we must maintain our why
	PCNAP	8		while we sleep a little
	LDY	PFRAME+3,U
	DEC	PFRAME+2,U
	BNE	L002_020
	BSR	LOCCLR		a final erase of the image area
	JMP	VSUCIDE		end the process
*
******************************************************************************
* X contains screen location  Y contains table location of image to be 'drawn'
*	(PFRAME,U)

* We should be good to run this code from here as the clifs to remove will be above this area somewhere from $0000-$7FFF
CLIFER:
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
  LDX	PFRAME,U
L002_021:
  LDA	,Y+	lo nybble is color, hi nybble is length
	BEQ	L001_047	length & color equ 0, end of image
	ANDA	#$0F
	LDB	#17	replicate lo nybble ($0A-=-$AA) for color
	MUL
	LDA	-1,Y
	ANDA	#$F0	length nybble is extracted
	LSRA		and adjusted
	LSRA
	LSRA
	LSRA
	BNE	L003_013	if length equ 0, end of line
	LDX	PFRAME,U	next line is always 1 down
	LEAX	1,X
	STX	PFRAME,U
	BRA	L002_021
L003_013:
	BEQ	L002_021	  (test is on top for minimal memory)
	CMPA	#1	last byte is always one pixel to enhance debris image
	BNE	L031_002	while all others are bytes for minimal memory
	ANDB	#$F0	requirements
L031_002:
* Store on CoCo 3 screen
  PSHS  A,X     * save the registers
  CMPX  #$9800      * If x co-ordinate is greater then 160 then don't draw it.
  BLS   >
  LEAX  -$F100,X    * I think this will set the value that starts $F145 to 0,1,2,...
!
  STB   SelfModCLIFER2+1   * Self mod below to save it for later
  TFR   X,D
* Change Joust Screen location to CoCo3 value in D
    STA     SelfModCLIFER+2
    LDA     #160              * 160 bytes per row
    MUL
SelfModCLIFER:
    ADDD    #$0000
    TFR   D,X   * X now has the CoCo 3 screen location
SelfModCLIFER2:
  LDB   #$FF      * Self mod from above
  STB	  ,X      * Draw it on CoCo 3 screen
  PULS  A,X     * restore them
	LEAX	$100,X	next location is over 1 as our images are horizontally
	CMPX	#$9800
	BHI	  L002_021
	DECA	oriented
	BRA	  L003_013
L001_047:
* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
	RTS
*
LOCCLR:
	LDB	WCLENY,Y		CALC LOWEST POINT
	EORB	#$04				;!WDMAFIX
	ADDB	WCY,Y
	JSR	VCL1ALL		clear cliff away
	LDD	#$1200		with a constant substitution of 00 color
	STD	WCDMA,X		in the DMA command
	LDY	PFRAME-2,U	the source is read for location and size
	LDD	WCSRC,Y		source is not use but is specified
	STD	WCSRC,X
	LDD	WCDEST,Y	destination
* Fix screen location width for CoCo 3
  CMPA  #$98
  BLO   >
  SUBA  #$F2      * match screen location for drawing depris
!
  CMPA  #05       * Make sure we clear the left side of the top left cliff
  BHI   >
  CLRA
!
	STD	PFRAME,U
	STD	WCDEST,X
	LDD	WCLEN,Y		vertical length is extended to cover
* Fix screen location width for CoCo 3
  EORA  #$04
  ADDA  #$02
  EORA  #$04
	EORB	#$04				;!WDMAFIX	the falling debris
	ADDB	#10+1
	EORB	#$04				;!WDMAFIX
	STD	WCLEN,X
	RTS
*******************************END OF CLIFF DESTROIER***********************
*
*	START PLAYER 1 & 2 IN TARGET AREA(S)
*
STPLY1
STPLY2	LDD	TARPLY		IS THIS SPOT EMPTY?
	BNE	L001_048		 BR=NO
	STU	TARPLY		IT IS NOW IN USE
	LDA	TARTIM		GIVE CURRENT TIME DELAY TIME
	STA	TARTM1
	RTS
L001_048:	STU	TARPL2		USING SECOND AREA
	LDA	TARTIM		GIVE CURRENT TIME DELAY TIME
	STA	TARTM2
	RTS
*
*	DEATH OF PLAYER 1
*
DEATH1	CLR	EGGS1
	INC	PLYD1		REMEMBER PLYR DIED IN THIS WAVE(SURVIVAL WAVE)
	BRA	SPDIE		REMOVE TARGET PLAYER POINTER
*
*	DEATH OF PLAYER 2
*
DEATH2	CLR	EGGS2
	INC	PLYD2		REMEMBER PLYR DIED IN THIS WAVE(SURVIVAL WAVE)
SPDIE	LDB	#5*60/8		BAITER REMOVAL TIME
	STB	DBAIT
	LEAX	,X		DEATH BY LAVA
	BEQ	SPDIE2		 BR=YES
	LDY	PDECSN,X	GET WINNING PLAYER/ENEMY DECISION BLOCK
	BEQ	SPDIE3		 BR=MUST BE A WINNING ENEMY
	LDY	DGLAD,Y		GET GLADIATOR ENTRY
	BEQ	SPDIE3		 BR=MUST BE AN ENEMY
	INC	,Y		KEEP TRACK OF PLAYER VS. PLAYER KILLS
	BGT	SPDIE2
	BEQ	SPDGLA		 BR=GLADIATOR WAVE, SCORE THE WINNING PLAYER
	DEC	,Y		(KEEP THE NBR POSITIVE)
	BRA	SPDIE2
*
SPDGLA	CLR	PLYG1		ONLY 1 GLADIATOR IN THE WAVE
	CLR	PLYG2
	LDA	#$80		INDICATE TRUE WINNING GLADIATOR
	STA	,Y
	LDX	PDECSN,X	GET WINNING GLADIATOR/PLAYERS SCORE AREA
	PSHS	X
	LDA	#$30		SCORE 3,000
	JSR	SCRHUN
	 LDX	#SCRAIR		DISPLAY SCORE ON THE SCREEN
	 LDD	#EGGID*256+$FF
	 JSR	VCUPROC
	LDD	PPOSX,U		PUT SCORE IN LOSERS AREA
	ADDD	#6		 (CENTER IN LOSERS KILL AREA)
	STD	PPOSX,Y
	LDD	PPOSY,U
	STD	PPOSY,Y
	LDA	#MSGTH3		DISPLAY 3,000 POINTS
	PULS	X		GET WINNERS DECSION TABLE BACK
	LDB	DCONST,X
	STD	PRDIR,Y
	LDX	#SNBOUN		AWARD BOUNTY SOUND
	JSR	VSND
	BRA	SPDIE2
*
*	PLAYER KILLED BY ENEMY OR PTERODACTYL
*
SPDIE3	LDA	GOVER		ATTRACT MODE?
	BPL	SPDIE2		 BR=YES
	LDA	TTROLL		1ST, 2ND, OR 3RD WAVE?
	BEQ	SPDIE2		 BR=NO
	LDA	#2
	STA	EMYTIM		SLOW DOWN THE ENEMY ONLY!
SPDIE2	LDX	#DEATH		create a primary process for player death
	LDD	#$2000		POOF PROCESS I.D.
	JSR	VCUPROC
	LDD	PPOSX,U
	STD	PPOSX,Y
	LDD	PPOSY,U
	STD	PPOSY,Y
	LDX	PDECSN,U	SCORE 50 POINTS FOR DYING
	LDA	#$50
	JSR	SCRTEN
	JSR	LZAP
*
	LDX	#SNPDIE		PLAYER DIES
	JSR	VSND
	CMPU	TARPLY		REMOVE PLAYER FROM TARGET AREA, CORRECT PLAYER?
	BNE	L001_049		 BR=NO
	LDD	TARPL2
	STD	TARPLY		HERE IF YES
	LDA	TARTM2
	STA	TARTM1
L001_049:	LDD	#0		ASSUME OTHER POINTER
	STD	TARPL2
*
*				CHECK FOR AN END OF GAME
*
L002_022:	LDX	PDECSN,U
	LDX	DSCORE,X	GET PLAYER SCORE/LIVES AREA
	DEC	6,X		SUBTRACT TO PLAYERS AUX NUMBER OF LIVES
	BNE	L005_005
L004_006:	LDA	GOVER		ATTRACT MODE?
	BPL	L005_005		 BR=YES, GAME IS ALREADY OVER
	LDA	SPLY1+6		BOTH PLAYERS DEAD?
	ORA	SPLY2+6
	BNE	L006_003		 BR=NO
	PSHS	U
	LDU	PDUMMY		PUT AT BEGINNING OF LIST
	 LDX	#GAMOVR
	 LDD	#$3400
	 JSR	VCUPROC
	PULS	U
L005_005:	RTS
L006_003:	 LDX	#GAMOV1		GAME OVER FOR PLAYER 1 OR 2
	 LDD	#PLYID*256+$FF
	 JSR	VCUPROC
	LDA	#3*60/8		3 SECOND MESSAGE
	STA	PJOYT,Y
	LDX	PDECSN,U
	STX	PDECSN,Y
	RTS
*
GAMOV1	LDX	PDECSN,U
	LDB	DCONST,X
	BSR	L010_023
	PCNAP	8
	LDA	GOVER		GAME OVER?
	BEQ	L001_050
	DEC	PJOYT,U
	BNE	GAMOV1
L001_050:	LDX	PDECSN,U
	CLRB
	BSR	L010_023
	JMP	VSUCIDE		end the process
*
L010_023:	LDA	DGAMO,X		PUT UP PLAYER 1/2 GAME OVER
	LDX	#$296C
	JSR	OUTPHR
	LEAX	$300,X		(SPACE CHARACTER)
	LDA	#MSGAMO
	JMP	OUTPHR
*
*	COLISION DETECT BETWEEN OPPONENTS VS ALL OPPONENTS
*	 (A SECONDARY PROCESS BEFORE COLISIONABLE PLAYERS)
*
OPPCOL	PCNAP	1		COLISION DETECT ROUTINE
L020_022:
L025_001:	LDU	PLINK,U		FIND TWO COLISIONAL OSTRICHES
	BEQ	OPPCOL		 BR= END OF COLISION
	LDA	PID,U
	BPL	L025_001
	STU	COLU
*
	LEAX	,U		FOUND 1ST OSRICH, FIND SECOND
L040_004:
L045_002:	LDX	PLINK,X
	BEQ	L020_022		BR= END OF PAIRS OF COLISION
	LDA	PID,X
	BPL	L045_002
	ANDA	PID,U
	BITA	#$01		ENEMY/PTERO VS. ENEMY/PTERO
	BEQ	L045_002
	BSR	HITEM
	BCC	L040_004 	THERE IS SOMETHING TO COLISION WITH
	BRA	L020_022
*
*	COLISION DETECT BETWEEN PLAYERS VS ALL OPPONENTS
*	 (A PROCESS BEFORE COLISIONABLE PLAYERS)
*
PLYCOL	PCNAP	1		COLISION DETECT ROUTINE
	LDA	TARTM1		DECREMENT TARGETED PLAYERS TIME
	BEQ	L010_024
	DEC	TARTM1
L010_024:	LDA	TARTM2		DECREMENT TARGETED PLAYERS TIME
	BEQ	L015_003
	DEC	TARTM2
L015_003:
L020_023:
L025_002:	LDU	PLINK,U		FIND TWO COLISIONAL OSTRICHES
	BEQ	PLYCOL		 BR= END OF COLISION
	LDA	PID,U
	CMPA	#PLYID+$80
	BNE	L025_002
	STU	COLU
	LEAX	,U		FOUND 1ST OSRICH, FIND SECOND
L035_002:	LDX	PLINK,X
	BEQ	PLYCOL		BR= END OF PAIRS OF COLISION
	LDA	PID,X
	BPL	L035_002
	BSR	HITEM
	BCS	L020_023
L040_005:
L045_003:	LDX	PLINK,X
	BEQ	L020_023		BR= END OF PAIRS OF COLISION
	LDA	PID,X
	BPL	L045_003
	BSR	HITEM
	BCC	L040_005		THERE IS SOMETHING TO COLISION WITH
	BRA	L020_023
*
*	COLISION DETECT THESE TWO OBJECTS (REG.U & REG.X)
*	 CARRY =1 FOR KILLING REG.U OBJECT
*	 REG.X & REG.U ARE INTACT
*
HITEM	STX	COLOBJ
	LDD	PPOSX,U		WITHIN X INTERSECTION SQUARES?
	SUBD	PCOLX,X
	BGT	HITEM1		 BR=NO
	LDD	PPOSX,X
	CMPD	PCOLX,U
	BGT	HITEM1
	SUBD	PPOSX,U
	STD	COLDX
*
	LDD	PCOLY1,X	WITHIN Y INTERSECTION SQUARES?
	CMPB	PCOLY1,U
	BHI	HITEM1		 BR=NO
	CMPA	PCOLY2,U
	BLO	HITEM1
	LDY	[PPICH,X]
	LDX	[PPICH,U]
	SUBB	PCOLY2,U
	ASLB			MAXIMUM Y DIFFERENCE = $1F
	ASLB
	BMI	OSTXP2		BR=X BOX ON TOP
	ABX
	BRA	OSTXYP
OSTXP2	NEGB
	LEAY	B,Y
OSTXYP	JSR	BPCOL
	BCS	OSTHIT
HITEM2	LDX	COLOBJ
HITEM1	ANDCC	#$FE
	RTS
*
*	PIXEL COLISION HAS HAPPENED, DECIDE WHAT TO DO WITH THE COLISION
*
OSTHIT	LDX	COLOBJ		GET OTHER COLISIONED PARTY
	LDA	PID,U		ENEMIES DO NOT KILL EACH OTHER
	ANDA	PID,X
	BITA	#$01
	BNE	L100_002		 BR=PTE/PLY/EMY COLISION
	BITA	#$02
	LBEQ	PLYEGG		 BR=CORRECT PERSON HIT EGG
	SWI			FICTISIOUS COLISION
L100_002:	BITA	#$04
	BNE	OSTHT2		 BR=NO KILL (ENEMY VS. ENEMY, PTERO VS. PTERO)
	BITA	#$08		PTERODACTYL VS. PLAYER?
	BNE	OSTBO		 BR=NO BUZZARD VS. PLAYER
	LDB	PLANTZ,U	LANTZ ON CORRECT LINE?
	ADDB	PPOSY+1,U
	SUBB	PPOSY+1,X
	LDA	PIMAGE,X			PFRAME,X
	CMPA	#FLY2-FLY1	ATTACKING FRAME?
	BLS	L013_001		 BR=NO WINGS DOWN FRAME
**PTE	SUBB	#7	15-7	FUDGE FROM LANTZ TO CENTER OF PTERO-MOUTH
	SUBB	#15-7		FUDGE FROM LANTZ TO CENTER OF PTERO-MOUTH
	BPL	L015_004
	NEGB
L015_004:	CMPB	#3		WITHIN 7 PIXELS?
	BRA	L014_000
*
L013_001:
**PTE	SUBB	#5	15-4	FUDGE FROM LANTZ TO CENTER OF PTERO-MOUTH
	SUBB	#15-5		FUDGE FROM LANTZ TO CENTER OF PTERO-MOUTH
	BPL	L010_025
	NEGB
L010_025:	CMPB	#2		WITHIN 5 PIXELS?
L014_000:	BHI	OSTBO		 BR=NO, CANNOT KILL PTERODACTYL
	LDA	PFACE,U		FACING IN OPPSITE DIRECTIONS?
	EORA	PFACE,X
	BPL	OSTBO		 BR=NO, CANNOT KILL PTERODACTYL
	LDD	COLDX		FACING PTERODACTYL?
	BPL	L012_006
	LDA	PFACE,U		PTERODACTYL ON LEFT, PLAYER ON RIGHT
	BPL	OSTBO		 BR=NO, CANNOT KILL PTERODACTYL
L011_006:	JMP	OSTPYP		BOUNCE KILLER AWAY FROM PTERODACTYL
*
L012_006:	LDA	PFACE,U		PTERODACTYL ON RIGHT, PLAYER ON LEFT
	BPL	L011_006		 BR=YES, KILLED PTERODACTYL
OSTBO	LDB	PLANTZ,U
	SEX
	STD	,--S
	LDB	PLANTZ,X	WHICH LANTZ IS ON TOP?
	SEX
	SUBD	,S++
	ADDD	PPOSY,X		 (DETERMINATION OF WHO IS KILLED)
	SUBD	PPOSY,U
	BEQ	L001_052		 BR=BOTH ON SAME LEVEL
	BMI	OSTXT3
	JMP	OSTPYP		 BR=REG.Y ON TOP
*
L001_052:	LDX	#SNPTHD		PLAYERS COLIDE
	JSR	VSND
	LDX	COLOBJ
	BRA	OSTXTT
*
OSTHT2	LDX	#SNETHD		ENEMIES COLIDE
	JSR	VSND
	LDX	COLOBJ
	LDA	PID,U
	CMPA	#$80+PTEID	PTERODACTYL?
	BEQ	OSTH12		 BR=YES
	LDA	PID,X
	CMPA	#$80+PTEID
	BEQ	OSTH13		 BR=YES
OSTH11	JSR	OSTBMP		NO-ONE DIES, BUT BUMP EACH OTHER ANYWAYS
	JMP	HITEM2
*
OSTH12	LDA	PID,X		2 PTERODACTYL'S?
	CMPA	#$80+PTEID
	BEQ	OSTH11		BR=YES, SAME OLD COLISION
OSTPX	JSR	PTEBRD		COLIDE PTERODACTYL & BIRD
	JMP	HITEM2
*				REG.U IS PTERODACTYL
OSTH13	EXG	X,U
	JSR	PTEBRD		COLIDE PTERODACTYL & BIRD
	EXG	X,U
	JMP	HITEM2
*
OSTBMP	LDB	PPOSY+1,X	DETERMINATION OF WHO HAS BEEN BUMPED
	SUBB	PPOSY+1,U
	LBPL	OSTUTP
	LBEQ	OSTLR
	JMP	OSTXTP
*
OSTXT3	LDX	COLOBJ		THE VICTOR!!!
	JSR	OSTWIN
	LDA	PID,X		IS THE WINNER A PTERODACTYL?
	CMPA	#$80+PTEID
	BEQ	OSTXPT
OSTXTT	JSR	OSTXTP		REG.X WILL BE RESTORIED
	BRA	OSTX12		REG.U GUY IS DEAD, GET NEXT REG.U GUY
*
OSTXPT	EXG	X,U
	JSR	PTEBRD		COLIDE PTERODACTYL & BIRD
	EXG	X,U
OSTX12	ORCC	#$01		REG.U GUY IS DEAD, GET NEXT REG.U GUY
	RTS
*
*	OSTRICH GENERAL KILL ROUTINE
*	 REG.X = THE WINNER
*	 REG.U = THE LOSER
OSTWIN	PSHS	X,U		NEVER DESTROY THIS WORKSPACE POINTER
	LDA	PID,U		MAKE THIS MAN DEAD
	ANDA	#$7F
	STA	PID,U
	LDD	#0
	STD	PRIDER,U
	LDY	PDECSN,U
	LDD	#AUTOFF		MOVE OSTRICH OFF SCREEN
	STD	PJOY,U
	JSR	[DDEAD,Y]	CALL DEATH ROUTINE (REG.X VICTOR, REG.U DEAD)
	LDU	2,S		RESTORE REG.U
	LDY	PDECSN,U	GET DEAD OBJECTS SCORE
	LDA	DVALUE,Y
	LDX	,S		RESTORE REG.X
	LDX	PDECSN,X
	JSR	[DVALUR,Y]	SCORE ROUTINE
	PULS	X,U,PC
*
OSTPYP	LDX	COLOBJ		GET THE LOSER
	EXG	X,U		SORT REGIES FOR DEATH WISH
	JSR	OSTWIN
	EXG	X,U		SORT REGIES FOR DEATH WISH
	LDA	PID,U		IS THE WINNER A PTERODACTYL?
	CMPA	#$80+PTEID
	BEQ	OSTPX
OSTYTT	BSR	OSTUTP		REG.X WILL BE RESTORIED
	JMP	HITEM2		REG.X IS DEAD, GET NEW REG.X
*
*	 BOUNCER ROUTINE
*	PLAYER VS. PLAYER & ENEMY VS. PLAYER & ENEMY VS. ENEMY
*	& PTERODACTYL VS. PTERODACTYL
*
OSTUTP	LDX	COLOBJ
	EXG	X,U
	JSR	OSTXUP		MOVE REG.U GUY UP
	EXG	X,U
	JSR	OSTXDN		 & REG.X GUY DOWN
	BRA	OSTLR
*
OSTXTP	LDX	COLOBJ
	EXG	X,U
	JSR	OSTXDN		MOVE REG.U GUY DOWN
	EXG	X,U
	JSR	OSTXUP		 & REG.X GUY UP
*
OSTLR	LDX	COLOBJ
	LDD	COLDX		NOW BOUNCE IN X DIRECTION
	BEQ	OSTNLR		 BR=CENTER TO CENTER COLISION
	BPL	OSTURT		BR=X ON RIGHT
OSTXLF	LDA	PVELX,X		U IS ON RIGHT, MOVE X LEFT
	BLE	L001_053		SO MAKE
	NEGA
	ADDA	#2		SLOW DOWN A BIT
	STA	PVELX,X
L001_053:	NEGA			BUMP OTHER PERSON
	ADDA	#2
	ASRA
	STA	PBUMPX,U
	LDA	#-1		MAKE X FACE LEFT
	STA	PFACE,X
	LDA	PVELX,U		U IS ON RIGHT, MAKE U GO RIGHT
	BGE	L002_023
	NEGA
	SUBA	#2		SLOW DOWN A BIT
	STA	PVELX,U
L002_023:	ADDA	#2		BUMP OTHER PERSON
	NEGA
	ASRA
	STA	PBUMPX,X
	CLR	PFACE,U		MAKE U FACE RIGHT
	BRA	OSTNLR
*
OSTURT	LDA	PVELX,X		X IS ON RIGHT, MAKE X GO RIGHT
	BGE	L001_054
	NEGA
	SUBA	#2		SLOW DOWN A BIT
	STA	PVELX,X
L001_054:	ADDA	#2		BUMP OTHER PERSON
	NEGA
	ASRA
	STA	PBUMPX,U
	CLR	PFACE,X		MAKE X FACE RIGHT
	LDA	PVELX,U		X IS ON RIGHT, MAKE U GO LEFT
	BLE	L002_024
	NEGA
	ADDA	#2		SLOW DOWN A BIT
	STA	PVELX,U
L002_024:	NEGA			BUMP OTHER PERSON
	ADDA	#2
	ASRA
	STA	PBUMPX,X
	LDA	#-1		MAKE U FACE LEFT
	STA	PFACE,U
*
OSTNLR	RTS
	FCB	$CE		LZAPPER FUDGE
*
*
OSTXUP	LDA	#-2
	STA	PBUMPY,X		X IS ON TOP, MAKE X GO UP
	LDD	PVELY,X
	BLE	L002_025
	COMA
	NEGB
	SBCA	#-1
	ASRA			SLOW DOWN A BIT
	RORB
	STD	PVELY,X
L002_025:	RTS
*
OSTXDN	LDA	#2
	STA	PBUMPY,X		U IS ON TOP, MAKE X LOWER
	LDD	PVELY,X
	BGE	L002_026
	COMA
	NEGB
	SBCA	#-1
	ASRA			SLOW DOWN A BIT
	RORB
	STD	PVELY,X
L002_026:	RTS
*
* Extra copy protection besides the ROM check
*
* After player dies code comes here and checks the ROM Checksum list if it doesn't match then it messes up a link list pointer [A008]
* This will eventually crash the game
*
LZAP:
	IF CopyProtect
	LDX	LZAP1
	ELSE
	RTS
	NOP
	NOP			; RTS and NOP,NOP keeps data in the same location, for testing
	ENDIF

	LDD	#34
L001_055:	ADDA	,X+
	DECB
	BNE	L001_055
	CMPA	#$32		LZAPPER DATA
	BEQ	L002_027
	LDX	[PFREE]
	INC	1,X
L002_027:	RTS
*
*	COLIDE PTERODACTYL & BIRD
*		REG.U = PTERODACTYL
*		REG.X = BIRD
PTEBRD	CLRA			DETERMINE WHO IS ON TOP
	LDB	PCOLY1,X
	SUBB	PCOLY1,U
	SBCA	#0
	ADDB	PCOLY2,X
	ADCA	#0
	SUBB	PCOLY2,U
	SBCA	#0
	BPL	L001_056		BR=PTERODACTYL IS ON TOP
	BSR	OSTXUP		BIRD IS ON TOP, MAKE HIM HIGHER
	LDA	#-5
	STA	PBUMPY,X	A HARD BUMP
	BRA	L002_028
*
L001_056:	BSR	OSTXDN		MAKE BIRD LOWER
	LDA	#5
	STA	PBUMPY,X	A HARD BUMP
L002_028:	LDD	PPOSX,X
	SUBD	PPOSX,U
	ADDD	PCOLX,X
	SUBD	PCOLX,U		WHO IS ON THE RIGHT OF WHO??
	BPL	L003_014		 BR=BIRD IS ON THE RIGHT, SO BUMP HIM RIGHT
	LDA	PVELX,X		THE BIRD MOVES LEFT NOW
	BLE	L021_005
	SUBA	#2		SLOW DOWN A BIT
	NEGA
	STA	PVELX,X
L021_005:	LDA	PVELX,U		BUMP BIRD
	NEGA
	ASLA
	STA	PBUMPX,X
	LDA	#$FF
	STA	PFACE,X		MAKE X FACE LEFT
	RTS
*
L003_014:	LDA	PVELX,X		THE BIRD MOVES RIGHT NOW
	BGE	L031_004
	NEGA
	SUBA	#2		SLOW DOWN A BIT
	STA	PVELX,X
L031_004:	LDA	PVELX,U		BUMP BIRD
	NEGA
	ASLA
	STA	PBUMPX,X
	CLR	PFACE,X		MAKE X FACE RIGHT
	RTS
LZAP1	FDB	$F320
*
*	LAVA FLAMES (BURNING THE BRIDGE)
*
LAVAB	PCNAP	4
	LDA	SAFRM2+1	LAVA TOPPED OFF?
	CMPA	#FLOOR
	BHI	LAVAB		 BR=NO, WAIT FOR LAVA TO RISE TO THE TOP
LAVABN	PCNAP	1
LAVABC	LDX	PDIST,U		MODIFY CLIFF5 LANDING AREA
	LDA	LNDXTB,X
	EORA	LNDXS3,X	GET XOR OF CORRECT BIT
	ANDA	#$20		GET BIT TO CHANGE
	EORA	LNDXTB,X	GET TRUE STATE OF BIT
	STA	LNDXTB,X	STORE IT AWAY!
	ANDA	#$20		HIT CLIFF5?
	BNE	L002_029		 BR=YES, STOP!!!
	LDB	PJOYT,U		MOVE FLAME
	LEAX	B,X
	STX	PDIST,U
	LDB	PVELX,U		TAKE CARE OF FLAME OFFSET
	LEAX	B,X
	CMPX	#ELEFT-4	TOO FAR LEFT?
	BLT	LAVABN		 BR=YES
	CMPX	#ERIGHT+4	TO FAR RIGHT?
	BGT	LAVABN		 BR=YES
	STX	PPOSX,U		NEW POSITION IN X DIRECTION
	INC	PFRAME,U
	LDA	PFRAME,U
	JSR	WFLAME		RE=WRITE FLAME
	PCNAP	6
	LDA	PFRAME,U
	JSR	CFLAME		ERASE FLAME
	INC	PFRAME,U
	LDA	PFRAME,U
	JSR	WFLAME		RE=WRITE FLAME
	PCNAP	6
	LDA	PFRAME,U
	JSR	CFLAME		ERASE FLAME
	BRA	LAVABC		LOOP ON EATING FLAME
*			END OF BRIDGE COLAPSE, SO DROP THE FLAME INTO LAVA
L002_029:	INC	PFRAME,U
	LDA	PFRAME,U
	JSR	WFLAME		RE=WRITE FLAME
	PCNAP	6
	LDA	PFRAME,U
	JSR	CFLAME		ERASE FLAME
	LDA	PFRAME,U
	ANDA	#$03
	BNE	L002_029
	INC	PPOSY+1,U
	LDA	PPOSY+1,U
	CMPA	#FLOOR+16	DOWN IN THE DEPTHS?
	BLO	L002_029		 BR=YES
	LDA	#$80		REFRESH CLIFF5
	ORA	BCKRFS
	STA	BCKRFS
	JMP	VSUCIDE		end the process
*
*	LAVA FLAMES (NORMAL)
*
LAVAF	LDA	PFRAME,U
	JSR	CFLAME
	INC	PFRAME,U	NEXT FRAME
	LDB	PPOSY+1,U	RAISE FLAME?
	CMPB	#FLOOR
	BLS	L001_057		 BR=NO
	DEC	PPOSY+1,U	HERE IF YES
L001_057:	LDA	PFRAME,U
	JSR	WFLAME		WRITE THE NEXT FLAME
	PCNAP	6
	BRA	LAVAF
*
WFLAME	PSHS	A		INPUT 0-3 FRAME NUMBER
	JSR	VWR1CLS
	BRA	WCFLAM
*
CFLAME	PSHS	A
	JSR	VCL1CLS
WCFLAM	PULS	A
	LDY	IFLAME_SHRAM
	ANDA	#3		COUNTS 0 TO 3
	ASLA
	LEAY	A,Y
	ASLA
	LEAY	A,Y
	JSR	WRHOR2
	JMP	CLIPER		CLIF THE BOTTOM OF THE FLAME
*
*	PUT THE NUMBER OF PLAYERS LIVES UP ON THE SCREEN
*	INPUT;
*	 REG.X HAS DECISION BLOCK ADDRESS (P1DEC OR P2DEC)
*	 REG.A HAS NUMBER OF LIVES TO PUT UP ON SCREEN
*	OUTPUT; NEW LIFES & SCREEN UPDATED IMMEDIATELY
*	 REG.X - INTACT
*
PLYRUP	PSHS	A		SAVE NUMBER OF LIVES
PLYRUL	JSR	INCLIV		ADD 1 LIFE TO PLAYER
	DEC	,S
	BNE	PLYRUL		GET NBR OF PLAYERS UP ON SCREEN
	PULS	A,PC
*
*	ADD 1 LIFE ON THE SCREEN
*	 REG.X HAS DECISION BLOCK
*	OUTPUT; NEW LIFE & SCREEN UPDATED IMMEDIATELY
*	 REG.X - INTACT
*
INCLIV	PSHS	X,CC		SAVE INTERUPT STATUS
	LDX	DSCORE,X	GET PLAYER SCORE/LIVES AREA
	BEQ	INLDON
	INC	5,X		ADD TO PLAYERS NUMBER OF LIVES
	INC	6,X		(ADD TO AUX NUMBER OF LIVES LEFT
	BNE	INLMAX		 BR=NOT EXCEEDED 255 LIVES
	DEC	5,X		WHAT I GIVETH, I TAKETH AWAY
	DEC	6,X
INLMAX	LDA	5,X
	CMPA	#5
	BHI	INLDON
	ASLA			CALC PROPER SCREEN LOCATION TO UPDATE
	ADDA	5,X
	SUBA	#3
	CLRB
	LDX	1,S		GET REG.X DECISION BLOCK BACK
	ADDD	DSCLOC,X
	ORCC	#$F0		NO INTERUPTS FOR DMA
;	STD	DDMA     * DESTINATION (WHERE DATA IS TO GO) 2-BYTES -Destination address
;	LDD	#$0703			;#$0307!XDMAFIX	CHARACTER SIZE
;	STD	WDMA     * HORIZONTAL SIZE (X-SIZE),VERTICAL SIZE (Y-SIZE)
;	LDD	DSCPLY,X	GET PLAYERS SOURCE CHARACTER ADDRESS
;	STD	SDMA     * ORIGIN (WHERE DATA IS FROM) 2-BYTES - Source address
;	LDA	#$0A		DMA CONTROL * Copied sprite to screen any zeros in the source are transparent
;	STA	DMA      * CONTROL REGISTER OF DMA
* Setup the correct block to draw the Extraman
    PSHS    A                 * We need the value in A (D points to the correct screen location)
    LDA     #Cliff5_And_Scoring
		STA			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table
    PULS    A                 * Restore the value in A (D points to the correct screen location)
    JSR     DrawExtraMan      * Draw score for player on screen
    LDA			#RegRAM6	        * A=Memory block $3E
    STA     MMU_Reg_Bank0_6   * Set Bank 6

INLDON	PULS	X,CC,PC
*
*	DECREMENT (BY 1) THE NUMBER OF LIVES A PLAYER HAS
*	 REG.X HAS DECISION BLOCK
*	OUTPUT; 1 LESS LIFE & SCREEN UPDATED IMMEDIATELY
*	 STATUS .EQ. NO MORE LIVES LEFT
*		.NE. MORE LIVES LEFT
*	 REG.X - INTACT
*
DECLIV	PSHS	X
	LDX	DSCORE,X	GET PLAYER SCORE/LIVES AREA
	BEQ	DCLNON
	LDA	5,X
	BEQ	DCLNON
	LDB	GOVER
	BPL	L010_026
	LDB	#7		BOOKS, NUMBER OF MEN PLAYED
	JSR	AUDIT1
L010_026:	DECA			1 LESS MAN LEFT
	STA	5,X		SAVE THIS AS NBR OF LIVES LEFT
	CMPA	#5-1
	BHI	DCLDON
	ASLA
	ADDA	5,X
	CLRB
	LDX	,S		GET REG.X DECISION BLOCK BACK
	ADDD	DSCLOC,X
	PSHS	CC		SAVE INTERUPT STATUS
	ORCC	#$F0		NO INTERUPTS FOR DMA
;	STD	DDMA      * DESTINATION (WHERE DATA IS TO GO) 2-BYTES -Destination address
;	LDD	#$0703			;#$0307!XDMAFIX	CHARACTER SIZE
;	STD	WDMA      * HORIZONTAL SIZE (X-SIZE),VERTICAL SIZE (Y-SIZE)
;	LDD	DSCPLY,X	GET PLAYERS SOURCE CHARACTER ADDRESS
;	STD	SDMA      * ORIGIN (WHERE DATA IS FROM) 2-BYTES - Source address
;	LDD	#$1A*256+(DKB*$11)	DMA CONTROL & ERASE CONSTANT
;	STB	KDMA      * CONSTANT WRITE REGISTER (make it dark brown)
;	STA	DMA       * CONTROL REGISTER OF DMA

* Setup the correct block to draw the Extraman
    PSHS    A                 * We need the value in A (D points to the correct screen location)
    LDA     #Cliff5_And_Scoring
		STA			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table
    PULS    A                 * Restore the value in A (D points to the correct screen location)
    JSR     EraseExtraMan      * Draw score for player on screen
    LDA			#RegRAM6	        * A=Memory block $3E
    STA     MMU_Reg_Bank0_6   * Set Bank 6
	PULS	CC
DCLDON	LDA	#1		STATUS NE
DCLNON	PULS	X,PC
*
*	GENERAL DMA PUTTER-UPPER
*	 (REG.X) HAS LENGTH, THEN SOURCE ADDRESS (X+2)
*	 REG.D HAS DESTINATION ADDRESS
*	 REG.Y HAS DMA CONTROL & CONSTANT
*
*  This routine is not used anymore
*
GENDMA	PSHS	CC
	ORCC	#$F0		NO INTERUPTS FOR DMA
	STD	DDMA
	LDD	,X++
	EORA	#$04			;#!HDMAFIX
	EORB	#$04			;#!HDMAFIX
	STD	WDMA
	STX	SDMA
	TFR	Y,D
	STB	KDMA
	STA	DMA
	PULS	CC,PC
*
*	FIND A FREE (NOT CROWDED) TRANSPORTER TO USE
*	 REG.X = ADDRESS OF FREE TRANSPORTER BOUNDS TABLE
*
FREET	PSHS	X
	LDD	CURTR1	CALCULATE FOR A FREE TRANSPORTER
	STD	STTR1
	LDD	CURTR3
	STD	STTR3
	LDU	[PDUMMY]	SEARCH THE LIST
CRENPR	LDU	PLINK,U		FOR OTHER FLYING OBJECTS
	BNE	CREID		 BR=NOT END OF LIST
	PULS	X,PC
CREID	LDA	PID,U		PROCESS ACTIVE ON SCREEN?
	BPL	CRENPR		 BR=NO
	CMPA	#$80+EGGID	AN EGG?
	BNE	L001_058		 BR=NO
	LDD	PDIST,U		CALLING A BIRD?
	BEQ	CRENPR		 BR=NO, SO IGNORE HIM
L001_058:	LDB	PVELX,U
	LDY	,S		GET TRANSPORTER BOUNDS TABLE
	LDX	,Y++		SPEED FUDGE TABLE
	LDD	B,X
	ADDD	PPOSX,U
	CMPD	#ELEFT
	BGE	L010_027
	ADDD	#ERIGHT-ELEFT
L010_027:	CMPD	#ERIGHT
	BLE	L020_024
	ADDD	#ELEFT-ERIGHT
L020_024:	TFR	D,X
	LDA	PPOSY+1,U
	CMPA	#$51+7		UPPER LEVEL?
	BHI	L030_012		 BR=NO
	LDA	STTR1		TRANSPORTER DISABLED?
	BNE	CRENPR		 BR=YES, NEXT PERSON
	BSR	TRBOND		CHECK IF THIS TRANSPORTER IS AVAILABLE
	BRA	CRENPR		NEXT PERSON
*
L030_012:	CMPA	#$8A+7		MIDDLE LEVEL?
	BHI	L040_006		 BR=NO
	LEAY	FTLEN,Y		 NEXT ENTRY
	LDA	STTR2		TRANSPORTER DISABLED?
	BNE	L035_003		 BR=YES, NEXT PERSON
	BSR	TRBOND		CHECK IF THIS TRANSPORTER IS AVAILABLE
L035_003:	LDA	STTR3		TRANSPORTER DISABLED?
	BNE	CRENPR		 BR=YES, NEXT PERSON
	LEAY	FTLEN,Y		 NEXT ENTRY
	BSR	TRBOND		CHECK IF THIS TRANSPORTER IS AVAILABLE
	BRA	CRENPR		NEXT PERSON
*
L040_006:	CMPA	#$A3+7		LOWER LEVEL?
	BLO	CRENPR		 BR=NO
	LDA	STTR4		TRANSPORTER DISABLED?
	BNE	CRENPR		 BR=YES, NEXT PERSON
	LEAY	FTLEN+FTLEN+FTLEN,Y	 NEXT ENTRY
	BSR	TRBOND		CHECK IF THIS TRANSPORTER IS AVAILABLE
	BRA	CRENPR		NEXT PERSON
*
TRBOND
	LDB	FXREV,Y		WRAP AROUND COMPARE?
	BNE	L010_028		 BR=YES
	CMPX	FXMIN,Y		IS OBJECT FAR AWAY ENOUGH TO LEFT?
	BLO	L030_013		 BR=YES
	CMPX	FXMAX,Y		IS OBJECT FAR AWAY ENOUGH TO RIGHT?
	BHI	L030_013		 BR=YES
	BRA	L020_025 	FAIL THIS TRANSPORTER
*
L010_028:	CMPX	FXMIN,Y		IS OBJECT FAR AWAY ENOUGH TO LEFT?
	BHI	L020_025		 BR=NO
	CMPX	FXMAX,Y		IS OBJECT FAR AWAY ENOUGH TO RIGHT?
	BHI	L030_013		 BR=YES
L020_025:	INC	[FLEVEL,Y]	NO, FAIL TRANSPORTER
L030_013:	RTS
*
*	ENEMY TRANSPORTER BOUNDS
*
TRENY	FDB	SPDFDG
	FCB	0				TOP TRANSPORTER
	FDB	$6A-66,$6A+14+66,STTR1
	FCB	0				MIDDLE RT TRANSPORTER
	FDB	$E0-66,$E0+14+39,STTR2
	FCB	1				MIDDLE LF TRANSPORTER
	FDB	$10-39+ERIGHT,$10+14+66,STTR3
	FCB	0				BOTTOM TRANSPORTER
	FDB	$78-66,$78+14+66,STTR4
*
*
*	PLAYERS TRANSPORTER BOUNDS
*
TRPLY	FDB	SPDFDG
	FCB	0				TOP TRANSPORTER
	FDB	$6A-90,$6A+14+90,STTR1
	FCB	1				MIDDLE RT TRANSPORTER
	FDB	$E0-90,$E0+14+90-ERIGHT,STTR2
	FCB	1				MIDDLE LF TRANSPORTER
	FDB	$10-90+ERIGHT,$10+14+90,STTR3
	FCB	0				BOTTOM TRANSPORTER
	FDB	$78-90,$78+14+90,STTR4
*
	FDB	-60,-30,-15,-8
SPDFDG	FDB	0,8,15,30,60		SPEED FUDGE
*
G1DEC	FDB	G1JOY,G1JOY,SPLY1,$39D9,SCOPL1,PLYR1_SHRAM,TREPL1,CREP1 PLAYER 1
	FDB	DEATH1,EGGS1,STPLY1,SCRHUN,PLYG1,PLYTIM
	FDB	SNPLWU,SNPLWD,SNPLSK,SNPLS2,SNPRU1,SNPRU2,SNPFAL,0,SNPCR1
	FCB	PL1*$11,$20,MSP1
G2DEC	FDB	G2JOY,G2JOY,SPLY2,$60D9,SCOPL2,PLYR2_SHRAM,TREPL2,CREP2 PLAYER 2
	FDB	DEATH2,EGGS2,STPLY2,SCRHUN,PLYG2,PLYTIM
	FDB	SNPLWU,SNPLWD,SNPLSK,SNPLS2,SNPRU1,SNPRU2,SNPFAL,0,SNPCR2
	FCB	PL2*$11,$20,MSP2
P1DEC	FDB	P1JOY,P1JOY,SPLY1,$39D9,SCOPL1,PLYR1_SHRAM,TREPL1,CREP1 PLAYER 1
	FDB	DEATH1,EGGS1,STPLY1,SCRHUN,PLYG1,PLYTIM
	FDB	SNPLWU,SNPLWD,SNPLSK,SNPLS2,SNPRU1,SNPRU2,SNPFAL,SNPTREF,SNPCR1
	FCB	PL1*$11,$20,MSP1
P2DEC	FDB	P2JOY,P2JOY,SPLY2,$60D9,SCOPL2,PLYR2_SHRAM,TREPL2,CREP2 PLAYER 2
	FDB	DEATH2,EGGS2,STPLY2,SCRHUN,PLYG2,PLYTIM
	FDB	SNPLWU,SNPLWD,SNPLSK,SNPLS2,SNPRU1,SNPRU2,SNPFAL,SNPTREF,SNPCR2
	FCB	PL2*$11,$20,MSP2
P3DEC	FDB	AUTOFF,AUTOFF,0000,$0000,SCOPL2,PLYR3_SHRAM,TREPL3,EMYDIE
	FDB	DEATH3,0,STENMY,SCRTEN,0,EMYTIM
	FDB	SNELWU,SNELWD,SNEMSK,SNEMS2,SNERU1,SNERU2,SNEFAL,0,SNECRE
	FCB	WHI*$11,$10,MSGAMO
P4DEC	FDB	LINET,BOUNDR,0000,$0000,SCOPL2,PLYR3_SHRAM,TREPL3,EMYDIE
	FDB	DEATH3,0,STENMY,SCRTEN,0,EMYTIM
	FDB	SNELWU,SNELWD,SNEMSK,SNEMS2,SNERU1,SNERU2,SNEFAL,0,SNECRE
	FCB	WHI*$11,$05,MSGAMO
P5DEC	FDB	LINET,B2UNDR,0000,$0000,SCOPL2,PLYR4_SHRAM,TREPL3,EMYDIE
	FDB	DEATH3,0,STENMY,SCRTEN,0,EMYTIM
	FDB	SNELWU,SNELWD,SNEMSK,SNEMS2,SNERU1,SNERU2,SNEFAL,0,SNECRE
	FCB	WHI*$11,$57,MSGAMO
P6DEC	FDB	LINET,SHADOW,0000,$0000,SCOPL2,PLYR5_SHRAM,TREPL3,EMYDIE
	FDB	DEATH3,0,STENMY,SCRHUN,0,EMYTIM
	FDB	SNELWU,SNELWD,SNEMSK,SNEMS2,SNERU1,SNERU2,SNEFAL,0,SNECRE
	FCB	WHI*$11,$15,MSGAMO
P7DEC	FDB	LINET,PTERO,0000,$0000,SCOPL2,0,TREPL3,EMYDIE
	FDB	DEATH4,0,STENMY,SCRHUN,0,EMYTIM
	FDB	SNELWU,SNELWD,SNEMSK,SNEMS2,0,0,SNEFAL,0,SNECRE
	FCB	WHI*$11,$10,MSGAMO
*
*	TRANSPORTER COLOR EFFECT
*
TREPL1	FCB	PL1*$11,PL1*$11,WHI*$11,PL1*$11,PL1*$11,GRY*$11,PL1*$11,PL1*$11
TREPL2	FCB	PL2*$11,PL2*$11,WHI*$11,PL2*$11,PL2*$11,GRY*$11,PL2*$11,PL2*$11
TREPL3	FCB	WHI*$11,WHI*$11,GRY*$11,WHI*$11,WHI*$11,GRY*$11,WHI*$11,WHI*$11
*
*	TRANSPORTER LOCATION TABLE
*
TR1ID	FDB	TRANS1_SHRAM,$6A+7,$51-1,CURTR1
TR2ID	FDB	TRANS2_SHRAM,$E0+7,$81-1,CURTR2
TR3ID	FDB	TRANS3_SHRAM,$10+7,$8A-1,CURTR3
TR4ID	FDB	TRANS4_SHRAM,$78+7,$D3-1,CURTR4
*
*	START THE ENEMY
*
STENMY	RTS
*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE
*
;	FCC	'JOUST (C) 1982 WILLIAMS ELECTRONICS INC.'
PLYTIM	FCB	1
*
*	PLAYER, & ENEMY FINALLY DIE
*
PLYDIE
EMYDIE	JMP	VSUCIDE		end the process
*
*	BEING CREATED/RE-CREATED
*
CREP1	EQU	*	* * * * CREATE PLAYER 1
CREP2	EQU	*	* * * * CREATE PLAYER 2
	LDX	PDECSN,U	GET DECISION BLOCK!
	JSR	DECLIV		DECREMENT NBR OF LIVES
	BEQ	PLYDIE
	LDA	NPSERV		TAKE A NUMBER TO BE SERVED BY THE TRANSPORTER
	STA	PTIMX,U
	INC	NPSERV
	BRA	CRELP
*
CREPLY:
	CMPB	LPSERV		IS IT THIS PLAYERS TURN?
	BNE	CRELP		    BR=NO, FAIL THIS GUY
	CLR	AREA1		    FIND A CLEAR AREA TO APPEAR FROM
	CLR	AREA2
	CLR	AREA3
	LDX	[PDUMMY]	  SEARCH THE LIST
L010_029:
  LDX	PLINK,X		  FOR OTHER FLYING OBJECTS
	BEQ	L040_007		BR=END OF LIST
	LDA	PID,X		    PROCESS ACTIVE ON SCREEN?
	BPL	L010_029		BR=NO
	CMPA	#$80+EGGID	AN EGG?
	BNE	L030_014		BR=NO
	LDD	PDIST,X		  CALLING A BIRD?
	BEQ	L010_029		BR=NO, SO IGNORE HIM
L030_014:
	JSR	SELARE		  CHECK IF AREA IS CLEAR
	BRA	L010_029
*
L040_007:
	LDD	CURTR1	    CALCULATE FOR A FREE TRANSPORTER
	STD	STTR1
	LDD	CURTR3
	STD	STTR3
	LDA	AREA3		    BOTTOM AREA EMPYT?
	BNE	L050_002		BR=NO
	LDA	STTR4		    BOTTOM TRANSPORTER NOT IN USE?
	BEQ	GOTR4		    BR=NO, GO USE IT
L050_002:
	LDA	AREA2		    MIDDLE AREA EMPYT?
	BNE	L060_000	  BR=NO
	LDA	STTR2		    RIGHT MIDDLE TRANSPORTER NOT IN USE?
	BEQ	GOTR2		    BR=NO, GO USE IT
	LDA	STTR3		    LEFT MIDDLE TRANSPORTER NOT IN USE?
	BEQ	GOTR3		    BR=NO, GO USE IT
L060_000:
	LDA	AREA1		    TOP AREA EMPYT?
	BNE	L070_000		BR=NO
	LDA	STTR1		    TOP TRANSPORTER NOT IN USE?
	BEQ	GOTR1		    BR=NO, GO USE IT
L070_000:
  LDX	#TRPLY		  TRANSPORTER SAFTEY AREA
	LDA	TRSMALL		  SMALLER TRANSPORTER AREA?
	BNE	CREALL		  BR=NO
	LDX	#TRENY		  NARROWER TRANSPORTER SAFTEY AREA
	BRA	CREALL		  GO CREATE VIA DEFAULT WAY
*
*	CREATE THE ENEMY (BOO HISS..)
*
CREEM	EQU	*	* * * * CREATE ENEMY
	LDA	NESERV		  TAKE A NUMBER TO BE SERVED BY THE TRANSPORTER
	STA	PTIMX,U
	INC	NESERV
CRELP	PCNAP	1
	LDB	PTIMX,U		  PLEASE HAVE YOUR NUMBER READY FOR THE OPERATOR
	LDA	PID,U
	CMPA	#PLYID		A NORMAL PLAYER?
	BEQ	CREPLY
	LDA	NPSERV		  ANY PLAYERS HOLDING A NUMBER?
	CMPA	LPSERV
	BNE	CRELP		    BR=YES, LET THEM FIND A TRANSPORTER 1ST
	CMPB	LESERV		IS IT THIS ENEMIES TURN?
	BNE	CRELP		    BR=NO, FAIL THIS GUY
	LDX	#TRENY		  TRANSPORTER SAFTEY AREA
CREALL:
	JSR	FREET
	JSR	VRAND
	BCC	GOTR12
	RORA
	BCS	GOTR4
	BRA	GOTR3
GOTR12:
	RORA
	BCS	GOTR2
GOTR1:
	LDX	#TR1ID
	LDA	STTR1	     TOP MOST TRANSPORTER?
	BEQ	GOTTR
GOTR2:
	LDX	#TR2ID
	LDA	STTR2	     NEXT MIDDLE TRANSPORTER?
	BEQ	GOTTR
GOTR3:
	LDX	#TR3ID
	LDA	STTR3	     NEXT MIDDLE TRANSPORTER?
	BEQ	GOTTR
GOTR4:
	LDX	#TR4ID
	LDA	STTR4	     BOTTOM TRANSPORTER?
	BEQ	GOTTR
	LDX	#TR1ID
	LDA	STTR1	     TOP MOST TRANSPORTER?
	BEQ	GOTTR
	LDX	#TR2ID
	LDA	STTR2	     NEXT MIDDLE TRANSPORTER?
	BEQ	GOTTR
	LDX	#TR3ID
	LDA	STTR3	     NEXT MIDDLE TRANSPORTER?
	BEQ	GOTTR
	LDX	#TR4ID
	LDA	STTR4	     BOTTOM TRANSPORTER?
	BNE	CRELP
GOTTR:
	INC	[TCURUSE,X]	CURRENTLY THIS TRANSPORTER IN USE
	LDU	PEXEC
	LDD	TPOSX,X		& INITIAL STANDING POSITION
	STD	PPOSX,U
	LDD	TPOSY,X
	STD	PPOSY,U
	STX	PSTATE,U	TEMPORARY TRANSPORTER LEVEL
	LDX	PDECSN,U
	LDX	DSNCRE,X
	LDA	PID,U
	CMPA	#PLYID
	BNE	L002_030
	INC	LPSERV	  ALLOW PLAYERS NEXT NUMBERED CARD TO USE TRANSPORTER
	BRA	L003_015
L002_030:
	INC	LESERV	  ALLOW ENEMIES NEXT NUMBERED CARD TO USE TRANSPORTER
L003_015:
	JSR	VSND
	LDA	#30
	STA	PFRAME,U
	LDY	PDECSN,U
	LDD	[DPLYR,Y]
	STD	PRIDER,U	PUT RIDER ON HORSE
	LDA	#STAND
	JSR	SRCADA
* Transporter Effect starts here
TREFF:
	LDX	   PSTATE,U          STATE OF PLAYER'S GROUND MOVEMENT (ADDRESS) X = 20,U = $85AD
	LDY	   [TIMAGE,X]        [0,X]  TIMAGE is SOURCE ADDRESS OF IMAGE    Y = [$85AD] = $0DD9 = TRANS1  FDB $0A00,TRASRC,$3551,$0A07
	JSR	   BCKYUP            PUT UP BACKGROUND, POINTED TO BY REG.Y  (WRITE THIS Sprite OBJECT) is three rows of $DD = Transporter colour
  IF UseCompiledSprites
  LDD    4,X               * get the Joust screen location - we might as well convert the address here outside of the IRQ so the IRQ is a tiny bit faster
;  DECB                     * Seems to draw it one row too low, this should fix it
  STA     TREFF_1+2        * Save A in selfmod ADDD below
  LDA     #160             * D = B * 160 (Y co-ordinate * 160)
  MUL
TREFF_1:
  ADDD    #$0000           * Self modified from above
  STD     4,X              * Write out CoCo 3 screen address for this Transporter to be drawn
	LDY	   PDECSN,U          JOYSTICK,SCORE, AND OTHER DECISION AREA (PRAM area)
	LDB	   DCONST,Y	         COLORED TRANSPORTER WHEN ACTIVE - set the colour of the transporter to COLOR NIBBLE OF PLAYER
  CMPB   #$11              * is it white = Enemy
  BNE    >
  LDD   #Transporter_White * point to Transporter with the Enemy Colour of white
  BRA   TREFF_5
!
  CMPB   #$55              * is it Player 1's colour?
  BNE   >
  LDD   #Transporter_Yellow * point to Transporter with Player 1's colour of Yellow
  BRA   TREFF_5
!
  LDD   #Transporter_Blue  * point to Transporter with Player 1's colour of Blue
TREFF_5:
  STD    2,X               * Save the address of the Transport compiled sprite code
  LDA    #%01001111        * Use this as a flag to indicate it's a coloured Transporter to draw
  LDB    #Sprites01        * We will use 1,X as the info for which MMU block to use for the sprites
  ELSE
	LDA	   ,X		             CONSTANT FILL OF TRANSPORTER  X = sprite block start
	ORA	   #$10              * Fill the transporter
	LDY	   PDECSN,U          JOYSTICK,SCORE, AND OTHER DECISION AREA (PRAM area)
	LDB	   DCONST,Y	         COLORED TRANSPORTER WHEN ACTIVE - set the colour of the transporter to COLOR NIBBLE OF PLAYER
  ENDIF
	STD	   ,X                Save new Sprite type and colour
	LDA	   PFRAME,U          Get the animation frame number for this object Counting down from 30
	CMPA	 #20                is it 20 yet?
	BGT	   TREFF2            If greater then 20 then go do Transporter effects #2 (First 10 animation frames skip effects below, Just highlighting transporter base)
*******************************************************************************
*
*  PATCHED-IN SPECIAL EFFECTS
*			by Kenneth F. Lantz  Williams Electronics May 1982
*
*******************************************************************************
	JSR	   WPLYR     WRITE OR CLEAR IMAGE - INPUT REG.X ADDRESS OF TOTAL IMAGE TO PUT TOGETHER
                   * X is moved to the next sprite info to process
	LDB	   PFRAME,U  PFRAME  RMB 1  MAJOR FRAME ANIMATION NUMBER   = $14 = 20  2nd B = $12 = 18
	TFR	   B,A       Animation number is going to be the height ??
	COMA	  	       VERT SIZE                                     = $EB , $ED
	ASRA             Divide it by two                              = $F5 , $F6
	ANDA	 #$0F      Make sure it's not taller then 15 pixels      = $05 , $06
	EORA	 #$04      Fix for DMA                                   = $01 , $02
	STA	   WCLENY,X  Height of this Sprite                         Saved as height
	TFR	   A,B       Save the xor4 height of the Sprite in B       Save in B
	CMPA	 #3        Is it XOR4 tst 3 pixels high?                 is A <= 3, no it's 1
	BLE	   L002_031  If it's 0,1 or 2 then skip ahead
	LDB	   #3        If it's taller then make the last sprite height=3 xor4= 7 pixels
L002_031:
	STB	   (WCLENY-10),X   WCLENY,X  Height of previous sprite  -10 is the previous sprite  Store 01 in height of previous sprite
  EORA	 #$04      Fix A so it's back to normal                 A = 5 again
	NEGA             A is a negative                              A = -5 = $FB
	INCA             A=A+1                                        A = $FC
	INCA             A=A+1                                        A = $FD
	ANDA	 #$0F      Make sure it's not taller then 15            A = $0D
	TFR	   A,B       Save B                                       Saved in B
	ADDA	 WCY,X      * WCY RMB 1 DMA DESTINATION Y PIXEL LOCATION A=A+$43=$50
	ADDB	 WCY-10,X   * add the value of the previous sprite (-10 is the previous sprite) B=B+$3D=$4A height of the previous sprite
	STA	   WCY,X      * Save the new Destination                   Save A=$50 as the new starting location for this sprite
	STB	   (WCY-10),X * Update the previous sprites                Save B=$4A as the new starting location for the previous sprite
*
	LDB	   WCLENY,X	  MAKE SURE BUZARD/OSTRICH LENGTH IS NOT TOO LONG  B=1
	EORB	 #$04       Fix width                                        B=5
	ADDB	 WCY,X                                                       B=B+$50=$55
	CMPB	 PPOSY+1,U                                                   Check B and the transporter
	BLS	   L003_016                                                    If it's less or the same skip
	LDB	   PPOSY+1,U                                                   otherwise B=the y of the transporter = $50
	INCB                                                               add one to the value B=$51
	SUBB	 WCY,X		  CALC NEW LENGTH THAT IS NOT BELOW TRANSPORTER    B=B-$50=1
	EORB	 #$04                                                        B=01 XOR 4
	STB	   WCLENY,X                                                    Save as height of the sprite
*
L003_016:
	LDY	PDECSN,U         PDECSN  RMB     2       JOYSTICK,SCORE, AND OTHER DECISION AREA  Y=$8981
	LDB	DCONST,Y         DCONST  RMB     1       COLOR NIBBLE OF PLAYER  B=$11
	LDA	WCDMA,X          Get type of Sprite      A = $2A
;	ORA	#$10             Flag it as a solid color  A=$3A
  ORA #%01000000      My special flag for transporter sprite
	STD	WCDMA,X          Save it                 Save A & B
	STD	WCDMA-10,X       Save it to previous sprite also

* 0000 TIMAGE  RMB     2               SOURCE ADDRESS OF IMAGE
* 0002 TPOSX   RMB     2               TRANSPORTER X PIXEL POSITION
* 0004 TPOSY   RMB     2               TRANSPORTER Y PIXEL POSITION
* 0006 TCURUSE RMB     2               TRANSPORTER "IN USE" FLAG ADDRESS               (    CoCoize20.asm):07406                 TCURUSE RMB     2               TRANSPORTER "IN USE" FLAG ADDRESS
*******************************************************************************
TREFF2:
	PCNAP	1		EFFECTS TIME    * Transporter is now white instead of grey
	JSR	   CNORIA    * erases the object
	LDA	   #$12      * Indicate a Block to draw   A=$12
	STA	   WCDMA,X   * Save it                    Save this type of sprite
	LDD	   PPOSX,U     PPOSX    RMB     3       PROCESS'S OBJECT X LOCATION  A = $00, B= $71
	ASRA         A=$00   A=A/2
	RORB         B=$38   B=B/2  D=D/2
	STB	   WCX,X     *  Save the new Destination X location    WCX     RMB     1       WRITE/CLEAR'S DMA DESTINATION X PIXEL LOCATION
	LDA	   #$0D					A=$0D - PROPER X WIDTH FOR BOX ERASE
	STA	   WCLENX,X     Write the width of the block to draw  WCLEN   RMB     0       WRITE/CLEAR'S DMA X,Y LENGTH (NO INVERSION)
	DEC	   PFRAME,U     PFRAME  RMB 1  MAJOR FRAME ANIMATION NUMBER
	LBNE	 TREFF        If the animation number is 0<> then go do Transporter Effect1 again
*
*	WAIT FOR 1ST MOVE, OR TIME OUT; WHICH EVER COMES FIRST
*
	LDX	PDECSN,U   PDECSN  RMB     2       JOYSTICK,SCORE, AND OTHER DECISION AREA
	LDD	DJOY,X		 SELECT PROPER JOYSTICK ROUTINE
	STD	PJOY,U
	LDA	#16*2
	STA	PFEET,U
	LSRA
	STA	PACCX,U
	LDA	#2
	STA	PLANTZ,U
	CLR	PTIMUP,U
	CLR	PTIMX,U
	BRA	L040_008
*
L020_026:	PCNAP	1
L040_008:
	JSR	[PJOY,U]	  GET JOYSTICK/DECISION
	LDD	CURJOY
	BNE	L051_000	  BR=PLAYER/ENEMY WANTS TO FLAP
	DEC	PTIMUP,U	  TIME TO CHANGE SPEEDS?
	BGT	L041_004
	LDA	#75
	STA	PTIMUP,U
	LSR	PFEET,U
	BEQ	L050_003
L041_004:
	JSR	WPLYR
	LDX	PSTATE,U	  CLIFF TRANSPORTER HI-LIGHT
	LDY	[TIMAGE,X]
	JSR	BCKYUP
	LDY	PDECSN,U
	LDY	DTREFF,Y
	LEAX	-10-10,X	BACKUP TO 1ST BUFFER
	LDB	PLANTZ,U
	BSR	L045_004
	DECB
	BSR	L045_004
	DECB
	BSR	L045_004
	DEC	PACCX,U		  CHANGE EFFECT
	BGT	L042_003
	INC	PLANTZ,U	  BLINK CLIFF TRANSPORTER ON/OFF
	LDA	PFEET,U
	STA	PACCX,U
L042_003:
	LDA	PFEET,U
	LSRA
	BNE	L020_026
	INC	PTIMX,U
	BRA	L020_026
*
L045_004:
	PSHS	B
	ANDB	#$07
	BNE	L047_002
	LDB	#2
	STB	PLANTZ,U
	STB	,S
L047_002:
	LDB	B,Y
	TST	PTIMX,U      * Check timer
	BEQ	L048_002     * If we hit zero then Fill player sprite with it's colour
	CMPB	#PL1*$11   * if its player 1's colour then skip
	BEQ	L049_001
	CMPB	#PL2*$11   * If its player 2's colour then skip
	BEQ	L049_001
L048_002:
  IF UseCompiledSprites
  JSR     HandleTransSprites
  LEAX    10,X            NEXT DMA BUFFER
  PULS    B,PC
  ELSE
	LDA	    WCDMA,X
  ORA     #$10
  STD     WCDMA,X
  ENDIF
 L049_001:
  LEAX    10,X            NEXT DMA BUFFER
  PULS    B,PC
*
L051_000:
	LDX	PDECSN,U	  ABORTED EARLY, KILL SOUND
	LDX	DSNTREF,X
	BEQ	L050_003		BR=NO SOUND
	JSR	VSND
L050_003:
	JSR	CPLYR
	LDX	PSTATE,U	  CLIFF TRANSPORTER BACK TO NORMAL
	DEC	[TCURUSE,X]	CURRENTLY THIS TRANSPORTER NOT IN USE
	LDY	[TIMAGE,X]
	JSR	BCKYUP
	LDU	PEXEC		    GET WORKSPACE BACK
	LDX	PDECSN,U
	JSR	[DSTART,X]	START THE ENEMIES AFTER THE PLAYER
	BSR	PLYINT		  RE-INCARNATED ENEMY/PLAYER
	LDD	CURJOY		  GIVE CURRENT JOYSTICK
	BRA	PLYRS2
*
*	INITILIZE SOME PLAYERS WORKSPACE
*
PLYINT	LDD	[DPLYR,X]
	STD	PRIDER,U	PUT RIDER ON HORSE
	LDD	#PLYBR		START AT GROUND STATE B
	STD	PSTATE,U
	LDD	#0		NO PVELY, (FOR INTELLIGENCE)
	STD	PVELY,U
	CLR	PVELX,U		EVERYONE STARTS FROMA STAND STILL
	CLR	PTIMX,U
	CLR	PFRAME,U
	CLR	PTIMUP,U
	CLR	PACCX,U
	CLR	PBUMPX,U	HAVE NOT BEEN BUMPED REGISTER
	CLR	PBUMPY,U
	LDA	#$80		ENABLE THIS PLAYERS COLISIONS
	ORA	PID,U
	STA	PID,U
	LDA	#STAND		STANDING POSITION
	JMP	SRCADG		GET CORRECT SOURCE ADDRESS FOR THE GROUND
*
*	PLAYER(S) MOVING RULES
*
PLAYR	EQU	*		PLAYER 1 & 2 START
	LDD	#LAND5		START Y POSITION
	STD	PPOSY,U
	PCNAP	60		WAIT A SECOND THEN..
	LDX	PDECSN,U	GET DECISION BLOCK!
	JSR	DECLIV		DECREMENT NBR OF LIVES
	LDU	PEXEC		GET WORKSPACE BACK
	LDX	PDECSN,U
	JSR	[DSTART,X]	START THE ENEMIES AFTER THE PLAYER
	LDX	PDECSN,U
	LDD	DJOY,X		SELECT PROPER JOYSTICK ROUTINE
	STD	PJOY,U
PLYLI2	BSR	PLYINT		RE-INCARNATED ENEMY
	JSR	WPLYR
*
*	MAIN RUNNING/STANDING/SKIDDING LOOP
*
PLYRLP	LDX	PDECSN,U
	LDA	[DTIME,X]
	JSR	VNAPTPC
	JSR	[PJOY,U]
PLYRS2
	TST	PTIMUP,U	FLAP BUTTON WAS RELEASED, WASNT IT?
	BNE	PLNFLY		 BR=YES
	TSTB			STARTING TO FLY??
	LBNE	STFLY		 BR=YES
PLNFLY	STB	PTIMUP,U	WAIT UNTIL BUTTON RELEASED
	LDB	PFACE,U		WHICH DIRECTION FACING?
	BPL	PLFRIT		 BR=RIGHT
	NEGA			REVERSE JOYSTICK FOR LEFT OPERATION
PLFRIT	LDB	PACCX,U		TIME TO UPDATE STATE TABLE?
	BEQ	UPDSTA		 BR=YES
	DEC	PACCX,U
	BNE	UPDNON		 BR=NO
UPDSTA	LDX	PSTATE,U
	ADDA	#4		GET NEXT OFFSET TO NEW STATE
	LDB	A,X		OFFSET TO NEXT STATE IN REG.B
	BEQ	UPDNON		 BR= SAME STATE
	LDA	#8		NEW STATE, WAIT FOR NEXT STATE
	STA	PACCX,U
	CLR	PTIMX,U		NEW STATES UPDATE POSITION
	LEAX	B,X
	STX	PSTATE,U
	CMPX	#PLYLR		START END OF SKID SOUND?
	BNE	UPDNON		 BR=NO
	LDX	PDECSN,U	HERE IF YES, GET PROPER SOUND
	LDX	DSNSK2,X
	JSR	VSND		MAKE END OF SKIDDING SOUND
UPDNON	DEC	PTIMX,U		TIME TO UPDATE POSITION?
	BLE	UPDNO2		 BR=YES
	LDB	PBUMPX,U		BEEN BUMPED?
	BEQ	PLYRLP		 BR=NO
	JSR	CPLYR		ERASE PLAYER
	CLRB
	BRA	PL2RIT		HERE IS YES
*
UPDNO2	JSR	CPLYR		ERASE PLAYER
	LDD	CURJOY		GET CURRENT JOYSTICK POSITION
	LDX	PSTATE,U
	LDA	6,X		UPDATE FICTISIOUS VELX FOR BUMPING
	STA	PVELX,U
	LDA	,X+		GET NEXT WAIT TIME
	STA	PTIMX,U		NEW WAIT TIME TILL NEXT STATE
	JSR	[,X]		GET REG.B=DELTA X, REG.X FRAME TO SHOW
	STA	PIMAGE,U	REMEMBER LAST FRAME
	LDA	PFACE,U
	BPL	PL2RIT
	NEG	PVELX,U
	NEGB			REVERSE DIRECTION FOR LEFT OPERATION
PL2RIT	JSR	WRAPX		ADD BUMP & WRAP AROUND IN THE X DIRECTION
	LDX	PPOSX,U		STILL WALKING ON THE CLIFF?
	LDY	PPOSY,U
	LDA	LNDXTB,X
	ANDA	LNDYTB+1,Y
	ANDA	#$7F		IGNORE LAVA TROLL
	LBEQ	STFALL		BR=NO, MUST BE FALLING
	LDA	BCKXTB+6,X	BUMPING INTO CLIFF3U??
	ANDA	BCKYTB,Y
	BEQ	PL2EDG		 BR=NO
	ORA	BCKRFS		REFRESH CLIFF 3U
	STA	BCKRFS
	CLR	PFACE,U		FACE LEFT
	LDA	PBUMPX,U
	BPL	PL2LF2
	NEGA
PL2LF2	ADDA	#3
	STA	PBUMPX,U
PL2EDG	BSR	SRCADH		GET CORRECT SOURCE ADDRESS
	BSR	WPLYR
	JMP	PLYRLP
*
*	CALC WRITE IMAGE SOURCE ADDRESS
*
SRCADG	STA	PIMAGE,U	COLISION DETECT ON THE GROUND
SRCADH	LDB	PPOSY+1,U
	STB	PCOLY1,U	A LARGE COLISION DETECT BOX
	SUBB	#$14-1		TOP LINE OF COLISION DETECTS BOX
	STB	PCOLY2,U
	BRA	SRCCOM
*
SRCADA	STA	PIMAGE,U	IN THE AIR IMAGE
SRCADR	LDB	PPOSY+1,U
	SUBB	#6		A SMALLER COLISION DETECT BOX
	STB	PCOLY1,U
	SUBB	#$14-1-6	TOP LINE OF COLISION DETECTS BOX
	STB	PCOLY2,U
SRCCOM	LDD	PPOSX,U
	ADDD	#18-1-1
	STD	PCOLX,U		MAINTAIN COLISION DETECT POINTERS
	CLR	PLANTZ,U	LANTZ OFFSET IS ZERO
	CLRB
	LDA	PFACE,U		FACE OFFSET
	BPL	SRCNOF
	LDB	#6
SRCNOF	ADDB	PIMAGE,U
	LDX	PHORSE,U	CALCULATE HORSE FRAME OFFSET
	ABX
	STX	PPICH,U
	LDX	PRIDER,U	CALCULATE RIDERS FRAME OFFSET
	BEQ	WPNORI		 BR=NO RIDER
	CMPB	#STAND		*FOR NOW, RUN1 TO FLY3 USES STANDING IMAGE
	BLO	SRCSKP		*
	SUBB	PIMAGE,U	*
	ADDB	#STAND		*
	ABX
WPNORI	STX	PPICR,U		STORE RESULTANT RIDER FRAME
	RTS
*
SRCSKP	ABX
	STX	PPICR,U		STORE RESULTANT RIDER FRAME
	LDB	#2		SKIDDING OSTRICH HAS A LANTZ 2 PIXELS LOWER
	STB	PLANTZ,U
	RTS
*
*	WRITE OR CLEAR IMAGE
*	INPUT REG.X ADDRESS OF TOTAL IMAGE TO PUT TOGETHER
*
WPLYR	LDU	PEXEC
	LDY	PPICR,U
	BEQ	WNORIA		BR=NO RIDER
	JSR	VWR2CLS		GET 2 DMA BLOCKS FOR RIDER/HORSE
WRRIDR	LDD	2,Y		WRITE PLAYER
	ADDA	CLSX
	ADDB	CLSY
	STD	WCDEST,X
	LDY	4,Y
	LDD	,Y++
	STD	WCLEN,X
	STY	WCSRC,X
	LEAX	10,X
*
WRHORS	LDY	PPICH,U		WRITE HORSE
WRHOR2	LDD	2,Y
	ADDA	CLSX
	ADDB	CLSY
	STD	WCDEST,X
	LDY	4,Y
	LDD	,Y++
	STD	WCLEN,X
	STY	WCSRC,X
WRRTS	RTS
*
WNORIA	LDY	PPICH,U		WRITE THE HORSE
	BEQ	WRRTS
WRREGY	JSR	VWR1CLS
	BRA	WRHOR2
*
*	CLEAR LAST IMAGE PLOTTED AT LAST PPOSX & Y
*	 (ORDER IS NOT IMPORTANT FOR ERASEING OBJECT)
*
CPLYR:          * ERASE PLAYER
	LDY	PPICR,U
	BEQ	CNORIA		BR=NO RIDER
	JSR	VCL2CLS		GET 2 DMA BLOCKS FOR RIDER/HORSE
	BRA	WRRIDR
*
CNORIA:
	LDY	PPICH,U		CLEAR THE HORSE
	BEQ	WRRTS
CLREGY:
	JSR	VCL1CLS
	BRA	WRHOR2
*
*	START TO FLY
*
STFLY	JSR	CPLYR		ERASE PLAYER
	LDD	#-$0080		INITIAL Y VELOCITY
	STD	PVELY,U
STKILL	DEC	PPOSY+1,U	JUMP UP 1 PIXEL (GET OUT OF LANDING AREA)
STFLY2	CLR	PPOSY+2,U	RESET FRACTIONAL DISTANCES
	CLR	PPOSX+2,U
	CLR	PBUMPY,U	RESET BUMP Y REGISTER
	LDD	#0
	STD	PSTATE,U	FLYING STATE
	LDD	#ADDGRA		NORMAL GRAVITY ROUTINE (NOT LAVA TROLLS)
	STD	PADGRA,U
	INC	PACCX,U		MAKE WINGS SHOW UP (1 FRAME), THEN DOWN
	JMP	FLAST2
*
*	START TO FALL
*
STFALL	LDA	PFRAME,U	A SKIDDING FALL OFF A CLIFF?
	BPL	L001_060		 BR=NO
	LDX	PDECSN,U	START FALLING SOUND (STOP SKIDDING SOUND)
	LDX	DSNFAL,X
	JSR	VSND
L001_060:	CLR	PPOSY+2,U	RESET FRACTIONAL DISTANCES
	CLR	PPOSX+2,U
	CLR	PBUMPY,U	RESET BUMP Y REGISTER
	LDA	#10
	STA	PACCX,U		SLOW TIME TO RAISE WINGS
	LDD	#0
	STD	PVELY,U		INITIAL Y VELOCITY
	STD	PSTATE,U	FLYING STATE
	LDD	#ADDGRA		NORMAL GRAVITY ROUTINE (NOT LAVA TROLLS)
	STD	PADGRA,U
	CLR	PTIMUP,U	NEEDS TO BE DONE
	LDA	CURJOY+1	FLAP BUTTON PRESSED?
	BNE	FLAPS2		 BR=YES
	BRA	FLIPS2		NO, START AT CORRECT FRAME
*
*	FLYING FLAP/FLIP LOOPS
*
*		WINGS ARE DOWN LOOP (FLAP PRESSED)
*
FLAPLP	JSR	WPLYR
	LDX	PDECSN,U
	LDA	[DTIME,X]
	JSR	VNAPTPC
	JSR	AIROVR		FLYING OVERHEAD
	TSTB			FLAP BUTTON STILL PRESSE?
	BEQ	GOFLIP		 BR=NO
FLAPS2	CLRB			NO OFFSET TO GRAVITY
FLAPST	LDX	#FLYX		BIRDS X VELOCITY TABLE
	JSR	[PADGRA,U]	ADD IN GRAVITY
	LDA	PACCX,U		MAINTAIN MINIMUM WING DOWN TIME
	BLE	WINGDN
	DEC	PACCX,U
WINGDN	LDA	#FLY1		WINGS DOWN
WINGFK	JSR	SRCADA
	JSR	CKGND		LANDED?
	BNE	FLAPLP		 BR=NO
	BRA	STLAN		START LANDING PROCEDURES
*
GOFLIP	LDX	PDECSN,U
	LDX	DSNWU,X		GET WING UP SOUND
	JSR	VSND
	CLR	PTIMUP,U	RE-INIT BUTTON PRESSES
	BRA	FLIPS2
*
*		WINGS ARE UP LOOP (FLAP RELEASED)
*
FLIPLP	JSR	WPLYR
	LDX	PDECSN,U
	LDA	[DTIME,X]
	JSR	VNAPTPC
	JSR	AIROVR		FLYING OVERHEAD
	TSTB			FLAP BUTTON STILL RELEASED?
	BNE	GOFLAP		 BR=NO
FLIPS2	LDB	#$04		OFFSET TO GRAVITY
FLIPST	LDX	#FLYX		BIRDS X VELOCITY TABLE
	JSR	[PADGRA,U]	ADD IN GRAVITY
	LDA	PACCX,U		MAINTAIN MINIMUM WING DOWN TIME
	BLE	L001_061
	DEC	PACCX,U
	LDA	#FLY1		PSYDO-WINGS DOWN FRAME (THEY ARE REALLY UP)
	BRA	GOTFIT
*
L001_061:	LDA	#FLY3		WINGS UP
GOTFIT	JSR	SRCADA
	JSR	CKGND		LANDED?
	BNE	FLIPLP		 BR=NO
	BRA	STLAN		START LANDING PROCEDURES
*
GOFLAP	JSR	ADDFLP		ADD IN NEW X & Y VELOCITIES
	CLRB			NO OFFSET TO GRAVITY (WINGS WILL BE DOWN)
	LDX	#FLYX		BIRDS X VELOCITY TABLE
	JSR	[PADGRA,U]	ADD IN GRAVITY
FLAST2	LDX	PDECSN,U
	LDX	DSNWD,X		GET WING UP SOUND
	JSR	VSND
	CLR	PTIMUP,U
	LDB	PACCX,U		SAVE MINIMUM WING DOWN TIME
	LDA	#5		WING DOWN MINIMUM TIME
	STA	PACCX,U
	TSTB			WERE WINGS ALLOWED TO GO UP?
	BLE	WINGDN		 BR=YES, SHOW NORMAL WINGS DOWN
	LDA	#FLY3		FAKE WINGS UP FRAME
	BRA	WINGFK
*
*	START LANDING PROCEDURES
*	 1 - DETERMINE RUNNING POSITION BASED ON VELOCITY X
*	 2 - DETERMINE RUNNING SPEED VIA TABLE LOOK UP (ROUNDING)
*	 3 - GIVE JOYSTICK POISTION FOR RUNNING DECISIONS
*
STLAN	LDA	PVELX,U		WHICH DIRECTION?
	BEQ	STLDIR		LAND IN SAME DIRECTION OF FLYING,
	CLR	PFACE,U		ASSUME FACING RIGHT
	TSTA
	BPL	STLDIR		LANDING RIGHT
	COM	PFACE,U		FACING LEFT
	NEGA			NEED A POSITIVE SPEED
STLDIR	LDX	#FRCONV		CONVERT FLYING TO RUNNING
	LDX	A,X		GET STATE
	STX	PSTATE,U		& REMEMBER IT
	CLR	PTIMX,U		ALSO CHANGE ANIMATION FRAME
	CLR	PACCX,U		IMMEDIATE LAND JOYSTICK SERVICE
	LDA	#1		MINIMUM DOWN TIME
	STA	PTIMUP,U
	LDD	#0		NO PVELY, (FOR INTELLIGENCE)
	STD	PVELY,U
	LDA	#STAND		LAND STANDING
	STA	PIMAGE,U
	JMP	PL2EDG		NO MORE FLYING
*
FRCONV	FDB	PLYBR	STAND STILL
	FDB	PLYCR	NAP 8 - SLOWEST SPEED
	FDB	PLYDR	NAP 4
	FDB	PLYER	NAP 2
	FDB	PLYFR	NAP 1 - FASTEST SPEED
*
ENDAD1	EQU	*	CURRENT END ADDRESS
;	IFGE	*-$DFFF
;	 FCB	$1111	ERROR, $D800-$DFFF MODULE IS TOO LONG
;	ENDIF
	PAGE
*
****************************************************************************

	ENDIF

**** ATT.SRC.out ****

*
* LINE DRAWING SUBROUTINE
*	Kenneth F. Lantz	c@ March 1982	William Electronics
*
;	NLIST
; 	INCLUDE RAMDEF.SRC
; 	INCLUDE EQU.SRC
; 	INCLUDE MESSEQU.SRC
;	LIST
*
*	COLOR PROCESSES WORK SPACE
*
	ORG	PRAM
PCOUNT	RMB	2	COUNTER TILL ALL OF NEW COLOR RAM IS DUMPED
PDUMP	RMB	1	COUNTER FOR HOW MANY BYTES ARE LEFT TO YSAVE
PCOLOR	RMB	2	POINTER TO CURRENT COLOR PALET
PYSAVE	RMB	2	POINTER FOR NEXT COLOR BYTE TO DUMP
*
	ORG	$BDD0	in scratch memory  ; Change for CoCo was 	ORG	$BC00
XLOC	FDB	0	range 0-304	!these four parameters set upon
YLOC	FDB	0	range 0-240	!entry, XLOC & YLOC are destroyed
ENDPTX	FDB	0	ditto		!
ENDPTY	FDB	0	ditto		!
CCOLOR	FCB	$11	set desired line color (left/right pixel respective)
	FCB	$11
FILDWN	FCB	0	none zero value here causes pixels to be filled in
*			in a downward direction until same color incountered
HCOUNT	FCB	0
COUNT	FDB	0
START	FDB	$5050
ERRCNT	FDB	0
ABSDX	FDB	0
ABSDY	FDB	0
QUAD	FDB	0
TAIL	FDB	$A0A0
SAVE	FDB	0
RLMASK	EQU	$40
TEMPM	FDB	0
CONTRL	EQU	$C900
OFFSET	FDB	$0000
FILL	EQU	$80
NOFILL	EQU	0
CL1	EQU	$11
CL2	EQU	$22
CL3	EQU	$33
CL4	EQU	$44
XPOS	EQU	4	OFFSET FROM ORGINAL START POSITION
***********************************
	ORG	$D000		in high memory due to screen access
MARQUE	PKILL	$00,$08		KILL H.S.T.D. PROCESSES
	LDX	VNULL		CHEAPIE BLANK THE SCREEN
	JSR	VDCOLOR
	PCNAP	2
	JSR	SCCLR		CLEAR SCREEN
	LDX	#MARCOL		MARQUE COLORS
	JSR	VDCOLOR
	JSR	OPWRT		WRITE THE OPERATORS MESSAGE
	LDD	#MSCOPY*256+$55	WILLIAMS COPYRIGHT MESSAGE  $55=mask
	LDX	#$1CBD     * Where to draw on screen
	JSR	OUTPHR
	LDY	#$1211		BACKGROUND FILL OF COLOR NIBBLE 11
	JSR	VDCRE2		DISPLAY CREDITS

  JSR GlenPatchCredits        * Go patch the credits block so it looks correct

	LDX	#REPLAY
	JSR	RCMSA
	TSTA			ANY EXTRA MEN ALLOWED?
	BEQ	LATT20_000		 BR=NO, SO SKIP THIS MESSAGE
	LDD	#MSW17*256+$33	EXTRA MAN AT XX,000 POINTS
	LDX	#$1FAB
	JSR	OUTPHR
	LDB	#$CC
	PSHS	X
	LDX	#REPLAY		GET REPLAY LEVEL
	JSR	RCMSA
	PULS	X
	BITA	#$F0
	BNE	LATT10_000
	ORA	#$F0
LATT10_000:	JSR	OUTBCD		DISPLAY THOUSANDS OF REPLAY POINTS
	LDA	#MSW18
	JSR	OUTPHR
LATT20_000:	PCNAP	1		DELAYS, DELAYS, WHEN DO WE SHIP IT!
	SECCR	STRIP,$11	STRIPE GENERATOR   ** DrawLOGOScreenBorder
	SECCR	FLASH,$10	COLOR RAM FLASH
* Draw big Joust logo
GO:
	LDU	 #LIST   * Set U to the start of the big Joust picture data
	CLR	 OFFSET  * Clear $BC1E we do an add with D below and we need this value to be zero
	LDA	,U+      * get the first byte of the data
	STA	OFFSET+1 * save it in $BC1F save the starting X Co-Ordinate
TOP:
  LDA	,U+      * Get the next byte of data (00)
	STA	FILDWN   * save it in $BC0A
	LDD	,U++     * Get the next two bytes of data
	STD	CCOLOR   * Store them in $BC08,$BC09=?colour? ($1111)
*
*  Draw Big Logo on screen
*
DEMO:
  CLRA         * A=0
	LDB	,U+      * B=next byte of data, U=U+1 ($02)
	ADDD	OFFSET	done this way to avoid the carry on an 8bit to 16bit +  * Add D with $00xx where xx is the byte of data read and saved to $BC1F, $BC1E always =$00
	STD	XLOC     * Save the new value in $BC00, this value divided by two is the X co-ordinate to start the line
	CLRA         * A=0
	LDB	,U+      * B=next byte of data, U=U+1 ($50)
	STD	YLOC     * Store $00xx where X is the last byte of data read store it @ $BC02, $BC03 is the Y co-ordinate
	CLRA         * A=0 (should already be zero)
	LDB	,U       * B=next byte of data ($03)
	BEQ	LATT01_000 * Test if we just read a zero byte if so skip ahead. otherwise get more data
	ADDD	OFFSET    * Add D with $00xx where xx is the byte of data read and saved to $BC1F, $BC1E always =$00
	STD	ENDPTX   * Save it @ $BC04
	CLRA         * A=0
	LDB	1,U      * B=1,U  ($64)
	STD	ENDPTY   * Save D @ BC06

* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    INCB
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank

	JSR	LINE     * Part of drawing the lines down the screen with data from $BC1A and $BC09 on the screen

* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
		INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4

	BRA	DEMO     * Loop again
*
*  Big LOGO done
*
LATT01_000:	LEAU	1,U
	LDB	,U+	a double zero ends a character
	BEQ	LATT02_000
	BRA	TOP
LATT02_000:	LDA	,U+
	BEQ	LATT09_000	another zero ends it all
	STA	OFFSET+1
	BRA	TOP
*
LATT09_000:
  LDU	PEXEC
	LDA	#111		111 * 10 = 1,110 = 18.5 SEC
	STA	PRAM,U
LATT20_001:	PCNAP	10
	DEC	PRAM,U
	BNE	LATT20_001
	PKILL	$00,$40		KILL ALL NON-ATTRACT MODE PROCESSES
	LDY	#RAMCOL+1
	LDD	#$FFFF
	STD	,Y++
	LDD	#0
	STD	,Y++
	STD	,Y++
	STD	,Y++
	STD	,Y++
	STD	,Y++
	STD	,Y++
	STA	,Y
	JMP	VSIM		INSTRUCTIONAL PAGE (GAME SIMULATION)
*
*	COLOR RAM FLASH GENERATOR
*
FLASH	LDD	#-1		INITILA COLOR CHANGE AFTER STRIPE ROUTINE
	STD	PCOUNT,U
	LDD	#MARCOL+8
	STD	PCOLOR,U
LATT10_001:	PCNAP	2
	LDX	RAMCOL+15
	LDD	PCOUNT,U
	BPL	LATT11_000
	LDY	PLINK,U
	LDA	PID,Y
	CMPA	#$11		STRIPE I.D.?
	BEQ	LATT20_002
	LDD	#16		INITIAL DELAY
LATT11_000:	BEQ	LATT17_000
	ADDD	#-1
	STD	PCOUNT,U
	BNE	LATT20_002
	LDY	PCOLOR,U
	LEAY	8,Y
	CMPY	#MAREND
	BLO	LATT15_000
	LDY	#MARCOL+8
LATT15_000:	STY	PCOLOR,U
	STY	PYSAVE,U
	LDA	#8
	STA	PDUMP,U
LATT17_000:	LDY	PYSAVE,U	IN MIDDLE OF RE-DUMPPING NEW COLOR
	LDX	,Y+
	STY	PYSAVE,U
	DEC	PDUMP,U
	BNE	LATT20_002
	LDD	#((((2*60+30)/16)+1)*8)+7 CHANGE COLORS EVERY 2 1/2 SECONDS
	STD	PCOUNT,U
LATT20_002:	LDD	RAMCOL+13
	STD	RAMCOL+14
	LDD	RAMCOL+11
	STD	RAMCOL+12
	LDD	RAMCOL+9
	STD	RAMCOL+10
	TFR	X,D
	LDB	RAMCOL+8
	STD	RAMCOL+8
	BRA	LATT10_001
*
*	STRIPE GENERATOR   ** DrawLOGOScreenBorder - Striped border
*
XLENS	EQU	16
YLENS	EQU	16
STRIP:
  CLR    PRAM+8,U
	LDX	   #$10-1
	LDY	   #$10
	LDA	   #8
LATT10_002:
	LEAX   1,X
	BSR    WRPIXH
	CMPX	 #$8F*2+XLENS
	BLO	   LATT10_002
	LEAX	 -XLENS,X
	LEAY	 YLENS-1,Y
LATT20_003:
  LEAY	 1,Y
	BSR	   WRPIXV
	CMPY	 #$F2
	BLO	   LATT20_003
	LEAY	-YLENS,Y
LATT30_000:
  LEAX	-1,X
	BSR	WRPIXH
	CMPX	#$0033*2
	BNE	LATT35_000
LATT35_000:
	CMPX	#$0
	BHI	LATT30_000
LATT40_000:
  LEAY	-1,Y
	BSR	WRPIXV
	CMPY	#$10
	BHI	LATT40_000
	JMP	VSUCIDE		end the process
*
WRPIXH:
  INCA
	ANDA	#$0F
	ORA	#$08
	STX	PRAM,U
	STY	PRAM+2,U
	STD	PRAM+4,U
	LDB	#YLENS
LATT10_003:
  BSR	WRANIB
	LEAY	1,Y
	INCA
	ANDA	#$0F
	ORA	#$08
	DECB
	BNE	LATT10_003
	BRA	WRPRTS
*
WRPIXV
  INCA
	ANDA	#$0F
	ORA	#$08
	STX	PRAM,U
	STY	PRAM+2,U
	STD	PRAM+4,U
	LDB	#XLENS
LATT10_004:
  BSR	WRANIB
	LEAX	1,X
	INCA
	ANDA	#$0F
	ORA	#$08
	DECB
	BNE	LATT10_004
WRPRTS
  LDD	,S++
	STD	PRAM+6,U
	DEC	PRAM+8,U
	BGT	LATT10_005
	LDA	#3
	STA	PRAM+8,U
	PCNAP	1
LATT10_005:	LDD	PRAM+4,U
	LDY	PRAM+2,U
	LDX	PRAM+0,U
	JMP	[PRAM+6,U]
*
WRANIB
  PSHS	D,X,Y
	LDA	#$0		READ SCREEN
	STA	DRRUC
	STA	RRUC
	TFR	X,D
	LSRA
	RORB
	TFR	B,A
	LDB	5,S
	TFR	D,X
	LDA	#$0F
	LDB	,S
	BCS	LATT10_006
	ASLB
	ASLB
	ASLB
	ASLB
	LDA	#$F0
LATT10_006:

    PSHS    A,B                 * Save A & B because we need to use them to convert Joust graphics location to CoCo3 graphics location
    TFR     X,D                 * D = X
    STA     ZD212_1+2           * Save A in selfmod ADDD below
    LDA     #160                * D = B * 160 (Y co-ordinate * 160)
    MUL
ZD212_1:
    ADDD    #$0000              * Self modified from above
    TFR     D,X                 * X now has proper CoCo3 screen location

* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    INCB
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank

    PULS    A,B                 * Restore A & B

  ANDA	,X
	BNE	LATT20_004
	ORB	,X
	STB	,X
LATT20_004:

* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
		INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4

	LDA	#$01		READ ROM!
	STA	DRRUC
	STA	RRUC
	PULS	D,X,Y,PC
*
*	INITIAL COLORS FOR THE MARQUE PAGE
*
MARCOL:
Palette4:                                             ;D224                 Palette Data Table4 Used for Big Joust Logo Screen
;         CoCo  Joust
    FCB    $00  ;$00                                  ;D224
    FCB    $00  ;$00                                  ;D225
    FCB    $24  ;$07                                  ;D226
    FCB    $36  ;$3F                                  ;D227
    FCB    $20  ;$05                                  ;D228
    FCB    $3F  ;$FF                                  ;D229
    FCB    $19  ;$E8                                  ;D22A
    FCB    $19  ;$E8                                  ;D22B
Palette4_8:
    FCB    $00  ;$00                                  ;D22C           * Middle of palette, will be cycled through when drawing Big Joust Logo (Border pattern changing)
    FCB    $00  ;$01                                  ;D22D
    FCB    $04  ;$03                                  ;D22E
    FCB    $20  ;$05                                  ;D22F
    FCB    $24  ;$07                                  ;D230
    FCB    $20  ;$05                                  ;D231
    FCB    $04  ;$03                                  ;D232
    FCB    $00  ;$01                                  ;D233
Palette4_Other:
    FCB    $00  ;$00                                  ;D234           * USed to cycle colours for pattern around the screen
    FCB    $00  ;$08                                  ;D235
    FCB    $02  ;$18                                  ;D236
    FCB    $10  ;$28                                  ;D237
    FCB    $12  ;$38                                  ;D238
    FCB    $10  ;$28                                  ;D239
    FCB    $02  ;$18                                  ;D23A
    FCB    $00  ;$08                                  ;D23B
    FCB    $00  ;$00                                  ;D23C
    FCB    $00  ;$00                                  ;D23D
    FCB    $01  ;$40                                  ;D23E
    FCB    $08  ;$80                                  ;D23F
    FCB    $09  ;$C0                                  ;D240
    FCB    $08  ;$80                                  ;D241
    FCB    $01  ;$40                                  ;D242
    FCB    $00  ;$00                                  ;D243
    FCB    $00  ;$00                                  ;D244
    FCB    $00  ;$09                                  ;D245
    FCB    $06  ;$1B                                  ;D246
    FCB    $30  ;$2D                                  ;D247
    FCB    $36  ;$3F                                  ;D248
    FCB    $30  ;$2D                                  ;D249
    FCB    $06  ;$1B                                  ;D24A
    FCB    $00  ;$09                                  ;D24B
    FCB    $00  ;$00                                  ;D24C
    FCB    $00  ;$09                                  ;D24D
    FCB    $07  ;$52                                  ;D24E
    FCB    $38  ;$A4                                  ;D24F
    FCB    $3F  ;$FF                                  ;D250
    FCB    $38  ;$A4                                  ;D251
    FCB    $07  ;$52                                  ;D252
    FCB    $00  ;$09                                  ;D253
Palette4_End:
;MARCOL	FCB	$00,$00,$07,$3F,$05,@377,$E8,@350
;	FCB	@000,@001,@003,@005,@007,@005,@003,@001
;	FCB	@000,@010,@030,@050,@070,@050,@030,@010
;	FCB	@000,@000,@100,@200,@300,@200,@100,@000
;	FCB	@000,@011,@033,@055,@077,@055,@033,@011
;	FCB	@000,@011,@122,@244,@377,@244,@122,@011
MAREND	EQU	*
*
*********************************************
*
DrawLineDownScreen:   * Draws a line down the screen with pattern from $BC1A ($F00F or $0FF0) and Colour from $BC09
FILLDN:
	PSHS	X	            This routine is entered with the current
	LEAX	1,X	          location in X and color in A

* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    INCB
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank

LATT01_001:
* Change Joust X (screen location to CoCo3 screen value)
    STX     SelfModX_D26B_1+1   * Self modify value below to restore X to Joust's version
    TFR     X,D                 * Move D=X
    STA     ZD256_1+2           * Save A in selfmod ADDD below
    LDA     #160                * D = B * 160 (Y co-ordinate * 160)
    MUL
ZD256_1:
    ADDD    #$0000              * Self modified from above
    TFR     D,X                 * X now has proper CoCo3 screen location

  LDD	SAVE
	ANDA	CCOLOR+1
	PSHS	A
	LDA	SAVE
	ANDA	,X
	BNE	LATT02_001
	ANDB	,X
	ORB	,S+
	STB	,X

SelfModX_D26B_1:
  LDX     #$0000          * Restore X - self mode from above

	EXG	D,X                * D = X
	INCB                   * D = D +1, move to the next byte down the screen
	EXG	X,D                * X = D
	BNE	LATT01_001         * Does B <> 0 then keep going, do another loop
	PSHS	A                 * save the X co-ordinate across the screen on the stack
LATT02_001:


  PULS	A,X,PC
*************************
* pixel movers  one pixel subroutines
*************************
LFTRT	LDA	#RLMASK
	BITA	QUAD
	BNE	LATT01_002	go right
	LDD	XLOC
	SUBD	#1
	BRA	LATT02_002
LATT01_002:	LDD	XLOC
	ADDD	#1
LATT02_002:	STD	XLOC
	RTS
UPDN	TST	QUAD
	BMI	LATT01_003	go down
	LDD	YLOC
	SUBD	#1
	BRA	LATT02_003
LATT01_003:	LDD	YLOC
	ADDD	#1
LATT02_003:	STD	YLOC
	RTS
******************************************
*  line drawing subroutine
***********************first calculate the quadrant and total distance
LINE:
	CLR	QUAD
	LDD	XLOC
	SUBD	ENDPTX
	ROR	QUAD
	BPL	LATT01_004
	COMA
	COMB
	ADDD	#1
LATT01_004:
  STD	ABSDX
	STD	ERRCNT
	LDD	YLOC
	SUBD	ENDPTY
	ROR	QUAD	bit7 set=down  bit6 set=right
	BMI	LATT02_004
	COMA
	COMB
	ADDD	#1
LATT02_004:	SUBD	#1
	STD	ABSDY
	CLR	HCOUNT
	COMA
	COMB
	CLRA
	ADDD	ABSDX
	BCC	LATT03_000
	INC	HCOUNT
LATT03_000:	STD	COUNT
	BRA	STP1
*  drawing loop follows *
MOVX	JSR	LFTRT
STP1	LDD	ERRCNT
	ADDD	ABSDY
COUNTS	STD	ERRCNT
	PSHS	CC
	CLR	DRRUC		THIS 1ST, BECAUSE INTERUPTS CAN CHANGE THIS
	CLR	CONTRL
	LDD	XLOC
	ASRA
	RORB
	TFR	B,A
	LDB	YLOC+1
	TFR	D,X
	LDB	#$F0
	BCC	LATT03_001
	COMB
LATT03_001:	TFR	B,A
	COMB
	STD	SAVE

* Change Joust X (screen location to CoCo3 screen value)
    STX     SelfModX_D31E_1+1     * Self modify value below to restore X to Joust's version
    STX     SelfModX_D325_1+1     * Self modify value below to restore X to Joust's version
    TFR     X,D                 * Move D=X
    STA     ZD312_1+2           * Save A in selfmod ADDD below
    LDA     #160                * D = B * 160 (Y co-ordinate * 160)
    MUL
ZD312_1:
    ADDD    #$0000              * Self modified from above
    TFR     D,X                 * X now has proper CoCo3 screen location
    STX     SelfModX_D323_1+1     * Save CoCo3 X value below, temporarily
    LDD     SAVE                * Restore A & B

	ANDA	CCOLOR
	ANDB	,X              * this is the CoCo 3's X
	PSHS	B
	TFR	A,B
	ORB	,S+
SelfModX_D31E_1:        * CoCo3 X - self mode from above - Restore Joust's X
  LDX   #$0000
	CMPX	#$9800
	BHS	RNGERR
SelfModX_D323_1:        * CoCo3 X - self mode from above - Read CoCo 3's X
  LDX   #$0000
	STB	,X
SelfModX_D325_1:        * CoCo3 X - self mode from above - Restore Joust's X
  LDX   #$0000
	TST	FILDWN
	BPL	LATT06_000
	JSR	FILLDN           * Draw pixels down the screen
LATT06_000:	LDA	#1
	STA	DRRUC		THIS 1ST, BECAUSE INTERUPTS CAN CHANGE THIS
	STA	CONTRL
	LDD	COUNT
	SUBD	#1
	BNE	LATT01_005
	TST	HCOUNT
	BEQ	DONE
	DEC	HCOUNT
LATT01_005:	STD	COUNT
	PULS	CC
	LBCS	MOVX        * was BCS	MOVX
MOVY	JSR	UPDN
	LDD	ERRCNT
	ADDD	ABSDX
	JMP	COUNTS         * was BRA	COUNTS
*
RNGERR	LEAS	2,S	pop one level and clean up the stack
DONE	PULS	CC,PC
*
**************************************************************************
*
***********************************************************
* O  -OH-    **************************************************
LIST	FCB	$40+XPOS
	FCB	NOFILL,CL1,CL1
	FCB	2,80,3,100,8,116,12,122,20,127,30,127,38,122,42,117
	FCB	48,100,48,80,0,1
	FCB	NOFILL,CL2,CL2
	FCB	28,92,28,85,30,80,32,75,0,1
*
	FCB	NOFILL,CL1,CL1
	FCB	32,75,35,86,35,94,0,1
*
	FCB	FILL,CL1,CL3
	FCB	14,86,14,95,17,104,22,112,27,112,33,104,36,94,0,1
*
	FCB	FILL,CL2,CL4,28,92,29,98,33,104,0,1
*
	FCB	FILL,CL1,CL4
	FCB	14,95,14,86,17,76,21,68,23,66,26,66,28,68,32,74,0,1
*
	FCB	FILL,CL1,CL3
	FCB	3,100,2,80,8,66,14,58,22,54,30,54,37,58
	FCB	44,66,48,80,0,1
	FCB	FILL,CL2,CL4,32,53,43,58,0,0
***** -J- *************************
	FCB	1+XPOS,NOFILL,CL1,CL1,9,109,12,110,22,122,32,127,41,126,47,123,52,121,0,1
	FCB	NOFILL,CL2,CL2,46,124,70,117,0,1
*
	FCB	FILL,CL1,CL4
	FCB	69,116,65,100,64,80,0,1
*
	FCB	FILL,CL1,CL4
	FCB	52,121,58,112,58,80,60,72,65,59,0,1
	FCB	FILL,CL2,CL4,65,59,71,65,0,1
*
	FCB	NOFILL,CL2,CL2
	FCB	30,76,44,76,0,1
*
	FCB	FILL,CL1,CL4
	FCB	30,75,44,70,0,1
*
	FCB	FILL,CL1,CL3
	FCB	30,75,30,70,65,59,0,1
*
	FCB	FILL,CL1,CL3
	FCB	44,70,44,104,42,108,36,111,31,110
	FCB	27,106,24,100,23,95,22,93,21,95,9,109,0,1
*
	FCB	FILL,CL2,CL4
	FCB	22,93,36,91,38,92,40,99,44,100,0,0
******************************************************
* U -YOU- ****************************************
	FCB	$6A+XPOS,NOFILL,CL2,CL3,1,118,13,107,0,1
	FCB	FILL,CL1,CL4,1,117,7,100,0,1
	FCB	NOFILL,CL1,CL4,10,88,10,98,12,106,0,1
	FCB	NOFILL,CL1,CL4,13,107,19,117,26,124,34,126,44,126
	FCB	51,124,54,122,0,1
	FCB	NOFILL,CL2,CL3,54,122,56,127,0,1
	FCB	NOFILL,CL1,CL3,57,118,56,128,63,126,68,126,76,128,0,1
	FCB	FILL,CL1,CL4,53,121,57,118,0,1
	FCB	FILL,CL1,CL3,74,126,71,114,70,84,0,1
	FCB	FILL,CL1,CL3,26,80,26,88,28,96,31,102,36,105,42,106,48,104
	FCB	53,100,56,90,57,77,58,66,0,1
	FCB	NOFILL,CL1,CL1,8,52,12,62,0,1,NOFILL,CL2,CL2,12,63,20,62,0,1
	FCB	NOFILL,CL1,CL1,78,50,72,64,70,84,0,1
	FCB	FILL,CL2,CL4,39,44,42,88,44,92,46,88,46,60,54,44,0,1
	FCB	FILL,CL1,CL4,54,44,56,54,57,66,57,77,0,1
	FCB	FILL,CL1,CL3,54,44,60,45,78,49,0,1
	FCB	FILL,CL1,CL4,26,80,28,70,34,54,39,44,0,1
	FCB	FILL,CL1,CL3,10,88,12,79,16,68,20,62,31,48,0,1
	FCB	FILL,CL1,CL4,13,62,20,54,31,48,0,1
	FCB	FILL,CL1,CL3,9,51,17,47,26,45,39,43,0,1
	FCB	FILL,CL2,CL4,0,58,16,68
	FCB	0,0
*  -S-  CEA  *
	FCB	166+XPOS,NOFILL,CL1,CL3,56,118,48,124,40,127,25,123,18,122,0,1
	FCB	NOFILL,CL2,CL4,18,122,12,114,0,1
	FCB	FILL,CL1,CL3,19,121,20,113,20,102,0,1
	FCB	FILL,CL1,CL4,20,102,18,90,0,1
	FCB	FILL,CL2,CL4,12,85,19,90,0,1
	FCB	FILL,CL1,CL3,18,90,34,106,41,108,44,104,44,99,0,1
	FCB	FILL,CL1,CL4,43,98,40,94,20,82,16,78,15,70,0,1
	FCB	NOFILL,CL2,CL4,56,118,68,123,0,1	*
	FCB	FILL,CL1,CL4,55,118,59,109,0,1
	FCB	FILL,CL1,CL3,58,108,56,99,50,88,32,76,30,70,0,1
	FCB	NOFILL,CL2,CL4,36,78,56,82,0,1
	FCB	FILL,CL1,CL4,55,81,48,72,37,66,32,66,30,70,0,1
	FCB	FILL,CL1,CL3,55,81,55,63,44,57,36,54,28,53,23,56,16,64,16,78,0,1

	FCB	FILL,CL2,CL4,13,63,21,58
	FCB	0,0
* -T- TEA  *
	FCB	218+XPOS,NOFILL,CL1,CL3,15,122,21,126,32,127,52,125,60,102,0,1
	FCB	NOFILL,CL1,CL3,30,96,40,78,0,1
	FCB	NOFILL,CL1,CL3,48,68,65,55,0,1
	FCB	NOFILL,CL2,CL4,40,78,60,76,0,1
	FCB	NOFILL,CL1,CL3,60,76,64,72,0,1
	FCB	FILL,CL1,CL4,41,77,45,75,60,75,0,1
	FCB	FILL,CL1,CL4,15,121,10,111,0,1
	FCB	FILL,CL1,CL3,11,110,11,104,14,96,21,81,32,72,0,1
	FCB	FILL,CL1,CL3,59,103,52,112,40,116,32,112,30,106,30,96,0,1
	FCB	FILL,CL2,CL4,59,102,47,99,42,102,36,106,30,106,0,1
	FCB	FILL,CL2,CL4,1,90,11,79,0,1
	FCB	FILL,CL1,CL4,11,79,26,74,31,73,0,1
	FCB	NOFILL,CL2,CL4,4,80,7,81,0,1
	FCB	FILL,CL2,CL4,4,65,9,62,0,1
	FCB	FILL,CL1,CL3,9,80,9,62,26,64,37,66,50,58,64,55,0,1
	FCB	FILL,CL1,CL3,48,68,63,72
	FCB	0,0
	FCB	0,0
*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE
*
;	FCC	'JOUST (C)1982 WILLIAMS ELECTRONICS INC.'
*
LENGTH_ATT	EQU	*		$D000 TO HERE
;	IFGT *-$D7FF
;	 FCB	$1111		OVERFLOWED $D000,$D7FF AREA
;	ENDIF
;	END	SYSV

****************************************************************************
*
*	PATCHES TO PREVENT PLAYER FROM PTERODACTYL HUNTING
*
	ORG	$D764	MODULE "ATT.SRC" ENDED AT $D6EF ON 10/29/82        *** Moved from $D760 to $D770
PATCHS	EQU	*	START ADDRESS OF PATCHES
;	IFLE	*-$D6EF
;	 FCB	$1111	ERROR, BUMPING INTO "ATT.SRC" MODULE AT $D6EF
;	ENDIF
*
*	FUDGE TO MAKE THIS PROM CHECKSUM CORRECTLY
*
	FCB	$F3	(ZAPPER) FUDGE TO GET THIS PART ($D000) ADD UP TO $3D
*
*	11TH PATCH, CLEAN UP GLADIATOR WAVE (GAME START GOOF)
*
PATC11	CLR	PLYG1		RESET 3,000 POINT TRIGGER
	CLR	PLYG2
	JMP	EMSGS		ERASE ANY END OF WAVE MESSAGES(OLD INSTRUCTION)
*
*	10TH PATCH, IF BAITER PTERODACTYL IS KILLED, SPEED THINGS UP!
*	 IF KILLED, THINGS REVERT BACK TO NORAML SPEED
*
PATC10	LDA	#1
	STA	EMYTIM		MAKE SLOW 1ST, 2ND, & OPTIONAL 3RD WAVE NORMAL
	CLR	PCHASE,U
	RTS
*
*	9TH PATCH, MAKE PTERODACTYL ACCOUNT FOR ITSELF DURING ATTACKS
*
PATCH9	TST	PCHASE,U	NON-BAITERS IGNORE THIS
	BEQ	L010_030
	DEC	PPVELX,U	CONTINUE NEXT SEEK TIMER
	BNE	L010_030		BUT SATURATE IT AT 1
	INC	PPVELX,U
L010_030:	DEC	PEGG,U		ATTACK SEQUENCE TIME OUT?
	RTS
*
*	8TH PATCH, MAKE THE 1ST PASS OF A BAITER PTERODACTYL MISS THE PLAYER
*		TO INDICATE A CHANGE IN INTELLIGENCE
*
PATCH8	TST	PCHASE,U	THIS ONLY APPLIES TO BAITERS
	BEQ	L010_031
	LDA	#138		DELAY TILL PTERODACTYL IS 1/2 WAY ACROSS SCREEN
L010_031:	STA	PPVELX,U	(RESTORE OLD INSTRUCTION)
	RTS
*
*	7TH PATCH, MAKE BAITERS COME OUT MORE QUICKLY, TYPICAL OLD TIME 4 MIN
*	  16 SEC, NEW TIME 2 MIN 16 SEC.
*				VERSION 4 SOFTWARE
*
PATCH7	EQU	*		NO CODE MODIFIED, JUST A TABLE
*
*	6TH PATCH, MODIFY PTERODACTYLS LINE TRACKING INTELLIGENCE TO SKIM
*		OVER HIGHER CLIF3U, THEN LOWER ITSELF TO CLIF3L & CLIF3R
*				VERSION 4 SOFTWARE
*
PATCH6	BLO	L010_032		BR=TRACK ON LINE 2 (RESTORE OLD 2 INSTRUCTIONS)
	LDA	#AOFFL3-2	ASSUME NEAR TRACKING LINE 3
	RTS
*
L010_032:	TST	PCHASE,U	AGAIN, THIS IS ONLY FOR BAITERS
	BEQ	L030_015
	LDD	PPOSX,U		TRACKING LINE 2, TO THE RIGHT OF CLIF3U?
	SUBD	#(260+7)
	BGT	L020_027		 BR=YES, GO TO LOWER TRACKING LINE
	SUBD	#(200-28-7)-(260+7)	TO THE LEFT OF CLIF3U?
	BGT	L030_015		 BR=NO, USE NORMAL UPPER CLIF3U LINE
L020_027:	LDA	#$88		NEW LINE TO TRACK FOR LOWER CLIFFS
	RTS
*
L030_015:	LDA	#AOFFL2-2	ASSUME NEAR TRACKING LINE 2
	RTS
*
*	5TH PATCH, SLOW DOWN 'Y' VELOCITY SO THAT ATTACKING PETRODACTYLS
*		DO NOT OVER SHOOT THEIR TARGET
*				VERSION 4 SOFTWARE
*
PATCH5	STA	PEGG,U		RESTORE OLD INSTRUCTION
	TST	PCHASE,U	AND AGAIN, THIS DOES NOT PERTAIN TO NON-BAITERS
	BEQ	L010_033
	ASR	PVELY+0,U
	ROR	PVELY+1,U
	ASR	PVELY+0,U
	ROR	PVELY+1,U
L010_033:	RTS
*
*	4TH PATCH, ADJUST PTERODACTYL TO HIT PLAYER IN THE LEGGS
*	 (PREVENT PLAYER FROM STANDING ON CLIF4 AND KILLING MY PETS)
*				VERSION 4 SOFTWARE
*
PATCH4	TST	PCHASE,U	HOWEVER, THIS PATCH IS NOT USED ON NON-BAITERS
	BEQ	L010_034
	ADDB	#2		JUST LOWER THE ATTACK WINDOW BY 2 PIXELS
L010_034:	STB	PRDIR,U		X=PDIST, Y=PRDIR (RESTORE OLD INSTRUCTION)
	RTS
*
*	3RD PATCH, USE LAVA TROLLS NEW VARIABLE GRAVITY (IN "ADDLAV")
*				VERSION 3 SOFTWARE
*
*				END OF 3RD PATCH
PATCH3	EQU	*		NO ROUTINES NEEDED
*
*	2ND PATCH, IF TIMER NOT EQUAL TO ZERO DECREMENT TIMER
*		   IF TIMER EQUAL TO ZERO, INCREASE LAVA TROLL PULL
*				VERSION 3 SOFTWARE
*
*
PATCH2	LDD	LAVKLL		CHECK LAVA TROOL TIMER
	ADDD	#-1
	BGT	L010_035
	LDD	CLVGRA
	CMPD	#$500		MAXIMUM LAVA TROLL GRAVITY
	BHI	L005_006
	ADDD	#1
	STD	CLVGRA
L005_006:	LDD	#1
L010_035:	STD	LAVKLL
	LDA	#1
	JMP	VNAPTIM
*
*	1ST PATCH, INITILIZE LAVA TROLL KILL TIME.
*		   INITILIZE LAVA TROLL VARIABLE GRAVITY
*				VERSION 3 SOFTWARE
*
*
PATCH1	STD	PPICH,Y		OLD INSTRUCTION
	LDD	#30*60		30 SECOND WAIT TILL TOUGHER LAVA TROLL
	STD	LAVKLL
	LDD	LAVGRA		GIVE CURRENT LAVA TROLL GRAVITY
	STD	CLVGRA
	RTS
*
;	IFGT	*-$D800
;	 FCB	$1111	ERROR, PATCH AREA ($D796-$D800) MODULE IS TOO LONG
;	ENDIF
PATCHL	EQU	*-PATCHS	LENGTH OF PATCHES
PATCHA	EQU	$D800-PATCHL	RECOMMENDED START ADDRESS
****************************************************************************
	PAGE
****************************************************************************

	ORG	$D800
*
*	SEARCH FOR A FREE AREA
*	 INPUT REG.X = WORKSPACE OF PROCESS WITH A VALID Y POSITION
*
SELARE	LDA	PPOSY+1,X
	CMPA	#$A3+6		$8A+6	BOTTOM AREA3 FREE?
	BLO	L001_062		 BR=YES
	INC	AREA3		HERE IF NO!
	RTS
*
L001_062:	CMPA	#$51		MIDDLE AREA2 FREE?
	BLO	L002_033		 BR=YES
	INC	AREA2		HERE IF NO!
	RTS
*
L002_033:	INC	AREA1		MUST BE IN TOP AREA1
	RTS
*
*	ADD IN THE FLAP TO FLIP VELOCITIES
*
ADDFLP	LDB	PTIMUP,U	NOW CALC GAINED Y VELOCITY
	LDA	#256*96/255	CALC NBR -96 TO 0 FROM 0 TO 255
	MUL
	TFR	A,B
	CLRA
	SUBD	#96		NEW DELTA-Y, USE IN GRAVITY CALC
	ADDD	PVELY,U
	STD	PVELY,U
	LDA	CURJOY		ADD IN JOYSTICKS LEFT OR RIGHT POSITION
	ASLA
	ADDA	PVELX,U		GET CURRENT TABLE VELOCITY-X SPEED
	BLT	ADXMAX
	CMPA	#MAXVX		PASSED MAXIMUM X VELOCITY?
	BGT	ADXMX2		 BR=NO
	STA	PVELX,U
	RTS
*
ADXMAX	CMPA	#-MAXVX		PASSED MINIMUM X VELOCITY?
	BLT	ADXMX2		 BR=NO
	STA	PVELX,U
ADXMX2	RTS
*
*	IN THE AIR OVERHEAD
*	 0 - GET JOYSTICK POSITION, (UPDATES CLEAR POINTERS)
*	 1 - FACE THE JOYSTICKS DIRECTION
*	 2 - INCREMENT FLAP/FLIP TIMER
*
AIROVR	JSR	[PJOY,U]		READ JOYSTICK
	TSTA			FACE RIGHT/LEFT??
	BEQ	AIRTIM		 BR=USE LAST DIRECTION
	CLR	PFACE,U		ASSUME RIGHT
	TSTA
	BPL	AIRTIM
	COM	PFACE,U		FACEING LEFT
AIRTIM	INC	PTIMUP,U	NBR OF TICKS IN THE AIR
	BNE	AIRRTS
	DEC	PTIMUP,U	NO OVERFLOW, JUST SATURATE
AIRRTS	JSR	CPLYR		ERASE PLAYER
	LDD	CURJOY		GIVE BACK JOYSTICK POSITION
	RTS
*
*	ADD IN GRAVITY
*	 INPUT REG.B = ADDITION TO GRAVITY (VELOCITY)
*		REG.X = X DRECTION VELOCITY TABLE
*	 1 - MODIFY VELOCITY Y FOR GRAVITY
*	 2 - CALC NEW POSITIONS
*
ADDGRA	ADDB	GRAV		ADD IN VARIABLE GRAVITY
	SEX
	ADDD	PVELY,U
	STD	PVELY,U		NEW Y VELOCITY
*
ADDGRX	ADDD	PPOSY+1,U	ADD IN FRACTIONAL DISTANCE
	ADDA	PBUMPY,U	ADD IN BUMPING REGISTER
	CLR	PBUMPY,U
	CMPA	#CEILNG		HIGHEST POSITION *****
	BHI	ADGCEI		 BR=HAVE NOT HIT CEILING
	INC	PBUMPY,U
	LDD	PVELY,U
	BPL	ADGDWN		 BR=ALREADY GOING DOWN, DON'T GO UP
	COMA
	NEGB
	SBCA	#-1		INVERT VELOCITY
	STD	PVELY,U
ADGDWN	LDD	#CEILNG*256
	BRA	ADGCEI
ADGCEI	CMPA	#FLOOR+7	BELOW SEA LEVEL (ON THE FLOOR)?
	BHS	ADGFLR		 BR=YES, TOO BAD, YOU ARE DEAD
	STD	PPOSY+1,U
*
	LDA	PVELX,U
	LDD	A,X
	ADDB	PVELX+2,U	ADD IN FRACTIONAL DISTANCE
	STB	PVELX+2,U
	ADCA	#0		CARRY TO POSITION X
	TFR	A,B
	JMP	WRAPX
*
*
*	DEATH VIA SWIMMING IN THE LAVA
*
ADGFLR
	LDX	#SNPLAV
	LDA	PID,U
	CMPA	#$80+PLYID	PLAYER
	BEQ	L001_063
	LDX	#SNELAV		ENEMY IN LAVA
L001_063:	JSR	VSND
ADGLAV	JSR	WPLYR		WRITE THE PLAYER
	JSR	FCLIP		CLIP THE IMAGE BASED ON THE FLOOR
	PCNAP	3
	JSR	CPLYR		ERASE THE PLAYER
	JSR	FCLIP		CLIP THE IMAGE BASED ON THE FLOOR
	LDA	PID,U		IS THE GUY DEAD OR JUST HERE FOR THE RIDE?
	BPL	ADGDED		 BR=DEAD
	LDB	WCLENY,X	HAS THE GUYS LENGTH CHANGED? (IN LAVA)
	CMPB	#$03			;7!X(!WDMAFIX) GUYS LENGTH IS ASSUMED TO BE 7 PIXELS TALL
	BEQ	ADGDED		 BR=NO, THE GUY IS STILL ABOVE THE LAVA
	ANDA	#$7F		NOW THIS GUY IS DEAD
	STA	PID,U
	LDD	#0
	STD	PPICR,U
	STD	PRIDER,U
	LDX	#0		NO VICTOR
	LDY	PDECSN,U
	JSR	[DDEAD,Y]	CALL DEATH ROUTINE (REG.X VICTOR, REG.U DEAD)
*
ADGDED	INC	PPOSY+1,U
	LDA	PPOSY+1,U
	CMPA	#FLOOR+20+1	ALL THE WAY IN THE LAVA?
	BLO	ADGLAV		 BR=NO, IT'S NOT SOUP YET!
	PCNAP	30
	LDX	PDECSN,U	TRY TO RE-CREATE THIS GUY
	JMP	[DCRE,X]
*
*	FLOOR CLIPPING ROUTINE FOR THE BIRDS
*
FCLIP	BSR	CLIPER		CLIP THE OSTRICH
	LDD	PPICR,U		IS A RIDER PRESENT?
	BEQ	CLPRTS		 BR=NO
	LEAX	-10,X		MUST BE, SO CLIP HIM ALSO
*
*	GENERAL CLIPPING ROUTINE FOR AFTER CLxCLS or WRxCLS WITH OFFSETS
*
CLIPER:
	LDA	  WCLENY,X    FIND BOTTOM LINE OF OBJECT  Bottom of object
	EORA	#$04				;!WDMAFIX
	ADDA	WCY,X                                   + The location of the object
	CMPA	#FLOOR+1  	BELOW THE FLOOR
	BLO	  CLPRTS		  BR=NO
	LDA	  WCY,X		    YES, IS THE TOP BELOW THE FLOOR?  A= Destination Y pointer
	CMPA	#FLOOR+1	  BELOW THE FLOOR
	BLO 	CLPIT		    BR=NO, JUST THE BOTTOM
	LDA	  #$F0		    WELL, WRITE THIS OFF SCREEN, A ROM
	STA	  WCX,X		    IS OK TO WRITE INTO (I HOPE)
	LDD 	#$0404			;#$0000!XDMAFIX	A 1 BYTE TRANSFER
	STD 	WCLEN,X
CLPRTS:
	RTS

CLPIT:
	IF UseCompiledSprites
  JMP   MyCLIPIT        Moved so we can test with mame
  ELSE
	NEGA
	ADDA	  #FLOOR+1	NEW LENGTH
	EORA	  #$04				;!WDMAFIX
	STA	    WCLENY,X
	RTS
  ENDIF

*
*	ADD IN LAVA TROLLS GRAVITY
*	 INPUT REG.B ADDITION TO GRAVITY (VELOCITY)
*	 1 - MODIFY VELOCITY Y FOR GRAVITY
*	 2 - CALC NEW POSITIONS
*
ADDLAV	CLR	PBUMPX,U	NO BUMPAGE ALLOWED
	CLR	PBUMPY,U
	CLR	PVELX,U		THE PLAYER IS NOT GOING ANYWERE
	SEX
	ADDD	CLVGRA		PATCH3
********	ADDD	LAVGRA
	ADDD	PVELY,U
	STD	PVELY,U		NEW Y VELOCITY
	CMPD	#-$0180		BREAK FREE VELOCITY?
	BLT	ADLFRE		 BR=NO, SO STOP FALLING
	ADDD	PPOSY+1,U	ADD IN FRACTIONAL DISTANCE
	STD	PPOSY+1,U
	CMPA	#FLOOR+7	BIRD IN THE LAVA?
	BLO	ADLX		 BR=NO
	LDX	PLINK,U		KILL LAVA TROLL TASKS
	LDA	PID,X		ANOTHER PROCESS CAN SNEAK IN,
	CMPA	#LAVID+1	SO, CHECK TO MAKE SURE THIS IS THE TROLL
	BNE	L011_007
	LDD	#LT2DIE		ERASE & STOP GRAB ROUTINE
	STD	PPC,X
L011_007:	LDX	PPREV
	LDD	#LT1DON		STOP RAISE/LOWER HAND ROUTINE
	STD	PPC,X
	LDD	#ADDGRA		BACK TO NORMAL GRAVITY
	STD	PADGRA,U
L022_003:	JMP	ADGFLR		NOW, THE GUY IS IN THE LAVA
*
ADLX	LDD	PPOSX,U		PUT THE GUY ON THE VISABLE SCREEN
	CMPD	#ELEFT+14+4
	BGT	L010_036
	LDD	#ELEFT+14+4
L010_036:	CMPD	#ERIGHT-14
	BLT	L011_008
	LDD	#ERIGHT-14
L011_008:	STD	PPOSX,U
	TFR	U,Y
	JSR	LAVVI3		BIRD OUT OF LAVA TROLLS RANGE?
	BNE	ADLFRE		 BR=YES
	RTS
*
ADLFRE	LDA	PID,U		IS THE PERSON ALIVE
	BPL	L033_001
	LDA	#$50		SCORE 50 POINTS FOR BREAKING FREE
	LDX	PDECSN,U
	JSR	SCRTEN
L033_001:	LDX	PLINK,U		KILL LAVA TROLL GRAB TASK
	LDA	PID,X		ANOTHER PROCESS CAN SNEAK IN,
	CMPA	#LAVID+1	SO, CHECK TO MAKE SURE THIS IS THE TROLL
	BNE	L011_009
	LDD	#LT2DIE
	STD	PPC,X
L011_009:	LDX	PPREV		DROP THE HAND
	LDD	#LT1DRP
	STD	PPC,X
	LDA	#1
	STA	PNAP,X
L022_004:	LDD	#ADDGRA		BACK TO NORMAL GRAVITY
	STD	PADGRA,U
	LDD	PVELY,U		SHOW NEW HIGHER POSITION
	ADDD	PPOSY+1,U	ADD IN FRACTIONAL DISTANCE
	STD	PPOSY+1,U
	RTS
*
*	CHECK IF ON THE GROUND, FROM PPOSX & PPOSY
*		STATUS NE = STILL IN THE AIR
*		       EQ = ON THE GROUND
*		OUTPUTS REG.B = Y LOCATION
*
CKGND	LDX	PPOSX,U		GET X LOCATION OF OBJECT
	LDY	PPOSY,U		GET Y LOCATION OF OBJECT
	LDA	LNDXTB,X	STARTING TO LAND ON A CLIFF?
	ANDA	LNDYTB,Y
	BNE	LND18		 BR=STARTING TO LAND(CANNOT COLIDE WITH BCKGND)
	LDA	BCKXTB,X	BACKGROUND MAP IN X DIRECTION
	ANDA	BCKYTB,Y	BACKGROUND MAP IN Y DIRECTION
	BEQ	CKG2		 BR=NO BOX COLLISION WITH BACKGROUND
	JSR	BCKCOL		PIXEL HIT BACKGROUND IMAGE?
CKG2	LDA	#1		NOT ON GROUND
	RTS
*
LND18	CMPA	#$08
	BLO	LND13
	BEQ	LNDB3
LND47	CMPA	#$20
	BLO	LNDB4
	BITA	#$20
	BNE	LNDB5
	BRA	LNDB7
*
LND13	CMPA	#$02
	BEQ	LNDB1
	BHI	LNDB2
*
LNDB0	EQU	*	CLIF1L & CLIF1R
	LDB	#$0045-1	ON TOP OF CLIFF PIXEL HEIGHT
	STB	PPOSY+1,U
	CLRA
	RTS
*
LNDB1	EQU	*	CLIF2
	LDB	#$0051-1	ON TOP OF CLIFF PIXEL HEIGHT
	STB	PPOSY+1,U
	CLRA
	RTS
*
LNDB2	EQU	*	CLIF3R
	LDB	#$0081-1	ON TOP OF CLIFF PIXEL HEIGHT
	STB	PPOSY+1,U
	CLRA
	RTS
*
LNDB3	EQU	*	CLIF3L & CLIF3R
	LDB	#$008A-1	ON TOP OF CLIFF PIXEL HEIGHT
	STB	PPOSY+1,U
	CLRA
	RTS
*
LNDB4	EQU	*	CLIF4
	LDB	#$00A3-1	ON TOP OF CLIFF PIXEL HEIGHT
	STB	PPOSY+1,U
	CLRA
	RTS
*
LNDB5	EQU	*	CLIF5
	LDB	#$00D3-1	ON TOP OF CLIFF PIXEL HEIGHT
	STB	PPOSY+1,U
	CLRA
	RTS
*
LNDB7	EQU	*	LAVA TROLLS
	LDA	TTROLL	LAVA TROLL ACTIVE?
	BNE	LNDB7C	 BR=NO
	LDA	LAVNBR	TOO MANY LAVA TROLLS?
	BNE	LNDB7C	 BR=YES, DO NOT START ANOTHER
	LDA	PID,U		CHOOSY MOTHER CHOOSE JIFF???
	CMPA	#$80+PLYID	PLAYER?
	BEQ	L001_064
	CMPA	#$80+EMYID	ENEMY?
	BNE	LNDB7C		ANY ONE ELSE, JUST IGNORE
L001_064:	INC	LAVNBR		1 MORE LAVA TROLL
	LDA	#LAVID		LAVATROLL I.D.
	LDB	PPRI,U		ALSO COPY PIRORITY VALUE
	LDX	#LAVAT1		START LAVA TROLL PROCESS
	LDU	PPREV		AFTER PREVIOUS PROCESS (BEFORE THIS ONE)
	JSR	VCUPROC		CREATE A PROCESS, ABRAKADABRA...
	LDU	PEXEC		HU HUM... BACK TO SAME OLD WORKSPACE
	STU	PJOY,Y		FINGER PRINT THIS PROCESS FOR THE LAVA TROLL
	CLR	PFRAME,Y	LAVA TROLL FRAME ZERO TO START
	LDD	#FLOOR-9	STARTING AT THE GROUND FLOOR
	STD	PPOSY,Y		 BUT NEXT WEEK HE'LL GET A PROMOTION
	LDD	PPOSX,U		START X POSITION
	ADDD	#-2		LAVA TROLL OFFSET
	STD	PPOSX,Y
	LDD	#0
	STD	PPICH,Y		NO PREMATURE CLEAR OF AN IMAGE
	LDA	#1		START FIRST FRAME
	STA	PJOYT,Y
LNDB7C	LDB	PPOSY+1,U	INDICATE NOT TO LAND
	RTS
*
*	DETECT & BOUNCE OFF BACKGROUND COLISION (FROM PPOSX & PPOSY)
*	INPUT:	REG.A - COLISION BIT OF ROUGH BACKGROUND COLISION
*		REG.U - THE OBJECT COLIDING WITH
*
BCKCOL	TFR	A,B
	ORA	BCKRFS		REFRESH THIS BACKGROUND IMAGE WHEN NECESSARY
	STA	BCKRFS
	TSTB
	LBMI	BCKB7
	CMPB	#$08
	BLO	BCK13
	LBEQ	BCKB3
BCK47	ANDB	#$70
	CMPB	#$20
	LBHI	BCKB6
	BLO	BCKB4
	JMP	BCKB5
*
BCK13	CMPB	#$02
	BEQ	BCKB1
	LBHI	BCKB2
*
BCKB0	EQU	*	BIT 0 = CLIF1L
	LDY	[CLIF1L_SHRAM]	COLISION DETECT POINTERS
	LDD	#-32
	LDX	#69
	BRA	BCKB04
*
BCKB4	EQU	*	BIT 4 = CLIF3L
	LDY	[CLIF3L_SHRAM]	COLISION DETECT POINTERS
	LDD	#-32
	LDX	#138
BCKB04	PSHS	Y
	JSR	BCKPIX
	BCC	NOBCK
	CMPY	,S++
	BNE	BCCOML
	LDA	1*4,X		TOP COLISION? ,LOWER 4 LINES OF BIRD/HORSE?
	BMI	L001_065		 BR=YES
	LDA	2*4,X
	BMI	L001_065		 BR=YES
	LDA	3*4,X
	BMI	L001_065		 BR=YES
	LDA	4*4,X
	BPL	BCCOML		 BR=NO, MUST OF HIT PLAYERS HEAD
L001_065:	JMP	BCTOP
*
BCCOML	LDA	4,Y		BOTTOM COLISION?
	BMI	BCBOT2		 BR=YES
	LDD	,Y		LEFT OR RIGHT SIDE?
	ADDD	2,Y		TO DECIDE TAKE A TYPE OF AVERAGE FOR THIS
	SUBD	,X		 COLISIONED LINE
	SUBD	2,X
	ADDD	COLDX
	ADDD	COLDX
	LBMI	BCRIT
	JMP	BCBOT		BR=OBJECT ON LEFT
*
NOBCK	LEAS	2,S
NOBCK2	RTS
*
BCKB1	EQU	*	BIT 1 = CLIF1R
	LDY	[CLIF1R_SHRAM]	COLISION DETECT POINTERS
	LDD	#252
	LDX	#69
	BRA	BCKB15
*
BCKB5	EQU	*	BIT 5 = CLIF3R
	LDY	[CLIF3R_SHRAM]	COLISION DETECT POINTERS
	LDD	#202
	LDX	#129
	JSR	BCKPIX
	BCC	NOBCK2
	LDB	PPOSY+1,U
	CMPB	#$89+4+6
	BHI	BCCOMR
	BRA	BCK15C
*
BCKB3	EQU	*	BIT 3 = CLIF3U
	LDY	[CLIF3R_SHRAM]	COLISION DETECT POINTERS
	LDD	#202
	LDX	#129
BCKB15	PSHS	Y
	JSR	BCKPIX
	BCC	NOBCK
	CMPY	,S++
	BNE	BCCOMR
BCK15C	LDA	1*4,X		TOP COLISION?, LOWER 4 LINES OF BIRD/HORSE?
	BMI	L001_066		 BR=YES
	LDA	2*4,X
	BMI	L001_066		 BR=YES
	LDA	3*4,X
	BMI	L001_066		 BR=YES
	LDA	4*4,X
	BPL	BCCOMR		 BR=NO, MUST OF HIT PLAYERS HEAD
L001_066:	JMP	BCTOP
*
BCCOMR	LDA	4,Y		BOTTOM COLISION?
	BMI	BCBOT2		 BR=YES
	LDD	,Y		LEFT OR RIGHT SIDE?
	ADDD	2,Y		TO DECIDE TAKE A TYPE OF AVERAGE FOR THIS
	SUBD	,X		 COLISIONED LINE
	SUBD	2,X
	ADDD	COLDX
	ADDD	COLDX
	LBPL	BCLFT		BR=OBJECT ON LEFT
BCBOT2	JMP	BCBOT
*
BCKB7	EQU	*	BIT 7 = CLIF5
	LDY	[CLIF5_SHRAM]		COLISION DETECT POINTERS
	LDD	#54
	LDX	#211
	BRA	BCKB27
*
BCKB2	EQU	*	BIT 2 = CLIF2
	LDY	[CLIF2_SHRAM]		COLISION DETECT POINTERS
	LDD	#86
	LDX	#81
	BRA	BCKB27
*
NOBCKB	LEAS	2,S
	RTS
*
BCKB6	EQU	*	BIT 6 = CLIF4
	LDY	[CLIF4_SHRAM]		COLISION DETECT POINTERS
	LDD	#106
	LDX	#163
BCKB27	PSHS	Y
	JSR	BCKPIX
	BCC	NOBCKB
	CMPY	,S++
	BNE	BCCOM
	LDA	1*4,X		TOP OF CLIFF?, LOWER 4 LINES OF BIRD/HORSE?
	BMI	BCTOP		 BR=NO
	LDA	2*4,X
	BMI	BCTOP		 BR=NO
	LDA	3*4,X
	BMI	BCTOP		 BR=NO
	LDA	4*4,X
	BMI	BCTOP		 BR=NO
BCCOM	LDA	4,Y		BOTTOM COLISION?
	BMI	BCBOT		 BR=YES
BCLR	LDD	,Y		LEFT OR RIGHT SIDE?
	ADDD	2,Y		TO DECIDE TAKE A TYPE OF AVERAGE FOR THIS
	SUBD	,X		 COLISIONED LINE
	SUBD	2,X
	ADDD	COLDX
	ADDD	COLDX
	BPL	BCLFT		BR=OBJECT ON LEFT
*
BCRIT	LDA	PVELX,U		MAKE VELOCITY X DIRECTION POSITIVE
	BGE	BCRITB
	NEGA
BCRITM	SUBA	#2		SLOW DOWN X VELOCITY
	STA	PVELX,U
BCRITB	ADDA	#6
	BRA	BCLFTV
*
BCBOT	LDD	PVELY,U		MAKE VELOCITY Y POSITIVE
	BPL	BCBOTM
	COMA
	NEGB
	SBCA	#-1
	ASRA
	RORB
	STD	PVELY,U
BCBOTM	BRA	BCLFTE
*
BCLFT	LDA	PVELX,U		MAKE VELOCITY X DIRECTION NEGATIVE
	BLE	BCLFTB
	NEGA
	ADDA	#2		SLOW DOWN X VELOCITY
	STA	PVELX,U
BCLFTB	SUBA	#6
BCLFTV	STA	PBUMPX,U
	LDD	PVELY,U		FALL A LITTLE BIT MORE
	CMPD	#-$0040
	BPL	BCLFTH
	LDD	#-$0040
BCLFTH	STD	PVELY,U
BCLFTE	LDA	#2
	STA	PBUMPY,U
	LDX	#SNCTHD
	JMP	VSND
*
BCTOP	LDD	PVELY,U		MAKE VELOCITY Y NEGATIVE
	BMI	BCTOPC
	COMA
	COMB
	ASRA
	RORB
	STD	PVELY,U
BCTOPC	LDA	#-2
	STA	PBUMPY,U
	LDA	PVELX,U
	BNE	BCUP		BOUNCE LEFT/RIGHT IF NO VELOCITY X
	LDD	,Y		LEFT OR RIGHT SIDE?
	ADDD	2,Y		TO DECIDE TAKE A TYPE OF AVERAGE FOR THIS
	SUBD	,X		 COLISIONED LINE
	SUBD	2,X
	ADDD	COLDX
	ADDD	COLDX
	BPL	BCTL		BR=OBJECT ON LEFT
	LDA	#2
	STA	PVELX,U
BCUP	LDX	#SNCTHD
	JMP	VSND
*
BCTL	LDA	#-2
	STA	PVELX,U
	LDX	#SNCTHD
	JMP	VSND
*
*	PIXEL BACKGROUND COLISION DETECT ROUTINE
*	 NEEDS; REG.U = PROCESS BLOCK WITH VALID PPOSX & PPOSY2 & PPICH
*		REG.Y = CLIFFS COLISION DETECT POINTER
*		REG.X = TOP LINE OF CLIFF (Y POSITION)
*		REG.D = LEFT COLUMN OF CLIFF (X POSITION)
*	OUTPUTS; REG.X = CURRENT OBJECTS COLISION POINTER
*		 REG.Y = CURRENT BACKGROUNDS COLISION POINTER
*		 STATUS C=0 (CC) - NO COLISION
*			C=1 (CS) - COLSION
*
BCKPIX	SUBD	PPOSX,U		GET DIFFERENCES IN X DIRECTION
	STD	COLDX		REMEMBER THIS DIFFERENCE
	TFR	X,D		FIGURE WHO IS ON TOP OF WHO
	LDX	[PPICH,U]	(AND GET OBJECTS COLISION DETECT DATA)
	SUBB	PCOLY2,U	COLISION DETECT UPON TOP LINE OF OBJECT
	BMI	BPYONX		 BR=Y (BCKGND) IS ON TOP OF X (OBJECT)
	ASLB			X (OBJECT) IS ON TOP, MATCH X LEVEL TO Y
	ASLB			(DIFFERENCE X4, OR 2 WORDS PER LINE)
	ROLA			 (MAXIMUM DIFFERENCE DELTA Y = $7F)
	LEAX	D,X
	BRA	BPCOL
*
BPYONX	NEGB			MAKE DIFFERENCE POSITIVE
	ASLB
	ASLB
	ROLA
	LEAY	D,Y
	BRA	BPCOL
*
BP1STA	LDA	,X
BP1STB	ORA	,Y
	RORA			CONTINUATION OR END OF TABLE POINTER?
	BCS	BPEN		 BR=END OF TABLE POINTER
	LEAX	4,X		NEXT POINTERS
	LEAY	4,Y
BPCOL	LDD	2,X
	BLT	BP1STB
	SUBD	,Y
	BVS	BP1STA
	SUBD	COLDX
	BLT	BPYX4		BR=NO COLISION BUT Y ON LEFT OF X
	LDD	,X
	SUBD	2,Y
	SUBD	COLDX
	BGT	BPXY4		BR=NO COLISION BUT X ON LEFT OF Y
	COMA			COLISION
	RTS
*
BPYX0	LEAX	4+4+4+4,X		Y ON LEFT OF X
	LEAY	4+4+4+4,Y		NEXT POINTERS
	LDD	2,X
	BLT	BPEN
	SUBD	,Y
	BVS	BPEN
	SUBD	COLDX
	BLT	BPYX4		BR=NO COLISION
	COMA			COLISION, Y ON LEFT OF X
	RTS
*
BPYX4	LDD	4+2,X
	BLT	BPEN
	SUBD	4+0,Y
	BVS	BPEN
	SUBD	COLDX
	BLT	BPYX8		BR=NO COLISION
	LEAX	4,X
	LEAY	4,Y
	COMA			COLISION, Y ON LEFT OF X
	RTS
*
BPYX8	LDD	4+4+2,X
	BLT	BPEN
	SUBD	4+4+0,Y
	BVS	BPEN
	SUBD	COLDX
	BLT	BPYXC		BR=NO COLISION
	LEAX	4+4,X
	LEAY	4+4,Y
	COMA			COLISION, Y ON LEFT OF X
	RTS
*
BPYXC	LDD	4+4+4+2,X
	BLT	BPEN
	SUBD	4+4+4+0,Y
	BVS	BPEN
	SUBD	COLDX
	BLT	BPYX0		BR=NO COLISION
	LEAX	4+4+4,X
	LEAY	4+4+4,Y
	COMA			COLISION, Y ON LEFT OF X
	RTS
*
BPEN	CLRA			NO COLISION
	RTS
*
BPXY0	LEAX	4+4+4+4,X		X ON LEFT OF Y
	LEAY	4+4+4+4,Y		NEXT POINTERS
	LDD	,X
	BLT	BPEN
	SUBD	2,Y
	BVS	BPEN
	SUBD	COLDX
	BGT	BPXY4		BR=NO COLISION
	COMA
	RTS
*
BPXY4	LDD	4+0,X
	BLT	BPEN
	SUBD	4+2,Y
	BVS	BPEN
	SUBD	COLDX
	BGT	BPXY8		BR=NO COLISION
	LEAX	4,X
	LEAY	4,Y
	COMA
	RTS
*
BPXY8	LDD	4+4+0,X
	BLT	BPEN
	SUBD	4+4+2,Y
	BVS	BPEN
	SUBD	COLDX
	BGT	BPXYC		BR=NO COLISION
	LEAX	4+4,X
	LEAY	4+4,Y
	COMA			COLISION, X ON LEFT OF Y
	RTS
*
BPXYC	LDD	4+4+4+0,X
	BLT	BPEN
	SUBD	4+4+4+2,Y
	BVS	BPEN
	SUBD	COLDX
	BGT	BPXY0		BR=NO COLISION
	LEAX	4+4+4,X
	LEAY	4+4+4,Y
	COMA			COLISION, X ON LEFT OF Y
	RTS
*
*
*	BIRDS X VELOCITY TABLE
*
	FDB	-$0200
	FDB	-$0100
	FDB	-$0080
	FDB	-$0040
FLYX	FDB	$0000
	FDB	$0040
	FDB	$0080
	FDB	$0100
	FDB	$0200
*
;STATE	MACRO	WAIT,CALL,MINUS,ZERO,PLUS,FLYVEL
;	FCB	WAIT,!HCALL,!WCALL,MINUS-*,ZERO-*,PLUS-*,FLYVEL
;	ENDM
STATE	MACRO	                     ; WAIT,CALL,MINUS,ZERO,PLUS,FLYVEL
	FCB	\1,\2/256,\2,\3-*,\4-*,\5-*,\6
	ENDM
PLYAR	STATE	8,REVR,PLYBR,PLYBR,PLYBR,0
PLYBR	STATE	0,STANDR,PLYAR,PLYBR,PLYCR,0
PLYCR	STATE	8,RUNR,PLYGR,PLYCR,PLYDR,2
PLYDR	STATE	4,RUNR,PLYCR,PLYDR,PLYER,4
PLYER	STATE	2,RUNR,PLYHR,PLYER,PLYFR,6
PLYFR	STATE	1,RUNR,PLYIR,PLYFR,PLYFR,8
PLYGR	STATE	2,RUNSR,PLYBR,PLYBR,PLYCR,2
PLYHR	STATE	2,SKIDR,PLYKR,PLYKR,PLYDR,4
PLYIR	STATE	2,SKIDR,PLYJR,PLYJR,PLYER,6
PLYJR	STATE	2,SKIDR,PLYKR,PLYKR,PLYDR,4
PLYKR	STATE	2,SKIDR,PLYLR,PLYLR,PLYDR,4
PLYLR	STATE	4,SKIDR,PLYMR,PLYMR,PLYCR,2
PLYMR	STATE	4,SKIDR,PLYBR,PLYBR,PLYCR,2
*
*	LEFT RIGHT TYPES OF MOVEMENTS
*
REVR	COM	PFACE,U		REVERSE DIRECTION
*
STANDR	LDD	#STAND*256+0		GET STANDING FRAME & DELTA POSITION
	CLR	PFRAME,U
	RTS
*
ORRUN	FCB	RUN1,0		INITIAL FRAME FROM STANDSTILL TO FRAME 1
	FCB	RUN4,3		PIXEL JUMP FROM FRAME 3 TO 4 (PFRAME 2 TO 1)
	FCB	RUN3,2		PIXEL JUMP FROM FRAME 2 TO 3 (PFRAME 3 TO 2)
	FCB	RUN2,1		PIXEL JUMP FROM FRAME 1 TO 2 (PFRAME 4 TO 3)
	FCB	RUN1,2		PIXEL JUMP FROM FRAME 4 TO 1 (PFRAME 1 TO 4)
*
RUNR	LDB	PFRAME,U	NEXT FRAME
	BLE	RUNRST		 BR=STARTING TO RUN
	DECB
	BGT	RUNFRM		BR=CORRECT FRAME NBR
	LDB	#4		START FRAME 4 THEN 3,2,1
RUNFRM	STB	PFRAME,U
	CMPB	#1		RUNNING THUMP FRAME?
	BNE	L001_067		 BR=NO
	LDX	PDECSN,U
	INC	PFEET,U		NEXT FOOT SOUND
	LDA	PFEET,U
	RORA
	BCC	L003_017
	LDX	DSNRU1,X	FIRST FOOT RUNNING SOUND
	BRA	L002_034
*
L003_017:	LDX	DSNRU2,X	SECOND FOOT RUNNING SOUND
L002_034:	JSR	VSND
	LDB	PFRAME,U
L001_067:	ASLB			X2
	LDX	#ORRUN		GET RUNNING INFORMATION.
	ABX
	LDD	,X		GET DELTA X DIRECTION & FRAME OFFSET
	RTS
*
RUNRST	LDB	#4		CURRENT FRAME
	STB	PFRAME,U
	LDD	ORRUN		STARTING RUNNING ANIMATION FRAME & FRAME OFFSET
	RTS
*
	FCB	0		STOP RUNNING OFFSET FROM A SKID
STPRUN	FCB	0		STOP RUNNING OFFSET FROM STANDING POSITION
	FCB	2		STOP RUNNING OFFSET FROM FRAME 4
	FCB	4		STOP RUNNING OFFSET FROM FRAME 3
	FCB	1		STOP RUNNING OFFSET FROM FRAME 2
	FCB	1		STOP RUNNING OFFSET FROM FRAME 1
*
RUNSR	LDX	#STPRUN		STOP RUNNING DELTA
	LDB	PFRAME,U
	LDB	B,X		GET CORRECT DELTA X FOR SKID
	LDA	#STAND
	CLR	PFRAME,U
	RTS
*
SKIDR	LDA	PFRAME,U
	BMI	L001_068		BR=ALREADY SKIDDING
	LDX	PDECSN,U
	LDX	DSNSK,X		GIVE SKIDDING SOUND
	JSR	VSND
L001_068:	LDA	#-1
	STA	PFRAME,U	-1 FRAME NBR
	LDD	#SKID*256+2	GET CORRECT DELTA X & PICTURE FOR SKID
	RTS
*
*	PLAYER 1 -READ JOYSTICK
*
P1JOY:
;  LDA	WCPIAB		SELECT HALF OF MUX
;	ORA	#$08
  LDA   WPIAA_P1 * Get P1 controls
	BRA	P2SAM		SAME AS PLAYER 2 JOYSTICK
*
*	PLAYER 2-READ JOYSTICK
*
P2JOY:
;	LDA	WCPIAB
;	ANDA	#$F7
;P2SAM:
;	STA	WCPIAB
;	LDA	WPIAA		READ JOYSTICK

  LDA   WPIAA_P2  * Get P2 controls
P2SAM:            * I moved the jump here
	CLRB			      ASSUME JUMP BUTTON NOT PRESSED
	BITA	#$04		  IS JUMP BUTTON PRESSED?
	BEQ	P2NJMP		  BR=NO
	INCB			      INDICATE JUMP PRESSED
P2NJMP	ANDA	#$03		CONVERT JOYSTICK TO + (RIGHT), 0, - (LEFT)
	ASRA			      VIA MATHAMATICAL EXPRESSION
	SBCA	#0
	STD	CURJOY		   STORE AS CURRENT JOYSTICK
	RTS
*
*	ADD BUMP,U OFFSET & WRAP AROUND IN THE X DIRECTION
*	 INPUT REG.B = NEW DELTA X POSITION
*
WRAPX	LDA	PBUMPX,U	ALSO ADD IN BUMP REGISTER
	BEQ	WRPWRD		NO BUMPING!
	BLT	WRPBLF		 BR=BUMPING LEFT
	CMPA	#3
	BLE	WRPBRM		BR=BUMP REMAINING TIME
	ADDB	#3
	SUBA	#3
	STA	PBUMPX,U
	BRA	WRPWRD
*
WRPBLF	CMPA	#-3
	BGE	WRPBRM		BR=BUMP REMAINING TIME
	ADDB	#-3
	SUBA	#-3
	STA	PBUMPX,U
	BRA	WRPWRD
*
WRPBRM	ADDB	PBUMPX,U
	CLR	PBUMPX,U		AND RESET BUMP REGISTER
*
WRPWRD	SEX			NEW DELTA DISTANCE
	ADDD	PPOSX,U		CHECK IF .EQ. OR BETWEEN -11 TO 296
	CMPD	#ERIGHT
	BLE	WRXMAX
	ADDD	#-(ERIGHT-ELEFT+1)
WRXMAX	CMPD	#ELEFT
	BGE	WRXMIN
	ADDD	#(ERIGHT-ELEFT+1)
WRXMIN	STD	PPOSX,U
	RTS
*
*	DYNAMIC CHANGES IN INTELLIGENCE
*
DYTBL
	DYWORD	$000B,$0006,$0004,-01,$0001,$FA86,$4433,$21	LAVTIM
	DYWORD	$0004,$0006,$0008,+02,$002D,$FA86,$4333,$21	LAVGRA
	DYWORD	$0010,$0008,$0004,-01,$0001,$FA86,$4433,$21	LAVLAV
	DYWORD	$0060,$0040,$0020,-02,$0010,$FFCA,$8643,$21	EGGWT
	DYWORD	$5A+4,$34+4,$1E+4,-04,$0008,$CA53,$A525,$21	EGGWT2
	DYWORD	$000E,$000E,$000C,-01,$000B,$FFFF,$FFF8,$42	BODNRG
	DYWORD	$F000,$F200,$F800,$20,$FF00,$4211,$4214,$21	BODNDI
	DYWORD	$0080,$0100,$0200,$20,$0300,$4211,$F813,$21	BODNVY
	DYWORD	$FFF1,$FFF2,$FFF4,+01,$FFF5,$FFFF,$FFF8,$42	BOUPRG
	DYWORD	$1000,$0E00,$0800,$E0,$0100,$4211,$4214,$21	BOUPDI
	DYWORD	$0003,$0002,$0001,-01,$0001,$FA86,$4433,$21	BOUPWD
	DYWORD	$000A,$0008,$0006,-01,$0002,$FA86,$4433,$21	BOUPWU
	DYWORD	$0020,$0015,$0010,-01,$0002,$7766,$4433,$21	BOLETM
	DYWORD	$000F,$000E,$000C,-01,$000B,$FFFF,$FFFA,$22	HUDNRG
	DYWORD	$F000,$F200,$F800,$20,$FF00,$FFFF,$FF84,$22	HUDNDI
	DYWORD	$0100,$0200,$0300,$20,$0380,$FFFF,$FF84,$84	HUDNVY
	DYWORD	$FFF1,$FFF2,$FFF4,+01,$FFF5,$FFFF,$FFF8,$22	HUUPRG
	DYWORD	$1000,$0E00,$0800,$E0,$0100,$FFF8,$FF84,$22	HUUPDI
	DYWORD	$0003,$0002,$0001,-01,$0001,$FFF8,$FF83,$22	HUUPWD
	DYWORD	$000A,$0008,$0006,-01,$0002,$FFF8,$FF83,$22	HUUPWU
	DYWORD	$FF80,$FF00,$FE00,$20,$FE00,$FFFF,$FF84,$84	HUUPVY
	DYWORD	$0020,$0015,$0010,-01,$0002,$FFF8,$8433,$22	HULETM
	DYWORD	$0014,$0010,$000E,-01,$000C,$FFFF,$FFFA,$82	SHDNRG
	DYWORD	$FFEC,$FFE0,$FFF2,+01,$FFF4,$FFFF,$FFFA,$82	SHUPRG
	DYWORD	$FFF0,$FE00,$FD00,$20,$FC00,$FFFF,$FFFA,$84	SHUPVY
	DYWORD	$0014,$000A,$0008,-01,$0002,$FFFF,$FFFA,$82	SHUPTM
	DYWORD	$0020,$0015,$0010,-01,$0002,$FFFF,$FFFA,$82	SHLETM
	DYWORD	$000A,$0008,$0006,-01,$0002,$FFFF,$FFFA,$61	SHCLTM
DYEND	EQU	*
*
ENDAD2	EQU	*	CURRENT END ADDRESS
;	IFGE	*-$DFFF
;	 FCB	$1111	ERROR, $D800-$DFFF MODULE IS TOO LONG
;	ENDIF

**** SYSTEM.SRC.out ****

; 	NAM SYSTEM OVERHEAD
;	SUBTTL	WILLIAMS 1982,
*
******************************************************************
*	WILLIAMS ELECTRONICS 1982
*	ORGINAL GAME NAME; JOUST
*	GAME DESIGNER; JOHN NEWCOMER
*	PROGRAMMER; BILL PFUTZENREUTER
*	STARTED PROGRAMMING; FEB 10, 1982
*	FILE: OVERHEAD SYSTEM PROGRAMS
*
;	INCLUDE	RAMDEF.SRC	DONT FORGET STANDARD DEFINITIONS
;	INCLUDE EQU.SRC		KOLKER EQUATES
DEBUG2	EQU	0	DEBUG
*
*  FILE I/O EQUATES
*
ERASE	EQU	$00		OFFSET TO ERASE AREA IN DMABEG & DMAEND
WRITE	EQU	$02		OFFSET TO WRITE AREA IN DMABEG & DMAEND
QUADA	EQU	$00		OFFSET TO QUADRANT A FROM DMABEG & DMAEND
QUADB	EQU	$40		OFFSET TO QUADRANT B FROM DMABEG & DMAEND
QUADC	EQU	$80		OFFSET TO QUADRANT C FROM DMABEG & DMAEND
QUADD	EQU	$C0		OFFSET TO QUADRANT D FROM DMABEG & DMAEND
*
*	THE PROGRAM'S VECTORS
*
* Initilize the Game
	ORG	SYSV
InitGame:
	ORCC	#$F0		DISABLE INTERUPTS
	LDS	#STACK		TRUE STACK
	JMP	INIT2		STANDARD OVERHEAD VECTORS
	JMP	CRPROC		CREATE A PROCESS
	JMP	KLPROC		KILL A PROCESS
	JMP	SUCIDE		KILL SELF PROCESS
	JMP	NAPTIM		NAP ENTERANCE
	JMP	DCOLOR		LOAD COLOR RAM ADDR OF COLORS
	JMP	RAND		RANDOM NUMBER GENERATOR
	JMP	CL1ALL		ALLOCATE DMA AREA FOR ERASE OPERATION
	JMP	WR1ALL		ALLOCATE DMA AREA FOR WRITE OPERATION
	JMP	CL1CLS		ALLOCATE & FLAVOR AREA FOR 1 ERASE OPERATION
	JMP	WR1CLS		ALLOCATE & FLAVOR AREA FOR 1 WRITE OPERATION
	JMP	CL2CLS		ALLOCATE & FLAVOR AREA FOR 2 ERASE OPERATION
	JMP	WR2CLS		ALLOCATE & FLAVOR AREA FOR 2 WRITE OPERATION
	JMP	SND		SOUND ROUTINE
	JMP	CUPROC		CREATE A PROCESS AFTER REG.U PROCESS
	FDB	COLOR1		COLOR TABLET ADDRESS
	FDB	LAVA		BUBBLING LAVA CONTROL ROUTINE
	JMP	NAPTPC		NAP VIA A JSR
	JMP	NEWCL5		THE NEW CLIF5 UN-COMPACTOR ROUTINE
*
	SETDP	BASE/256
INIT2:
		LDA			 #BASE/256       ; Bits: 1010 0000    ;E03D    86A0					* SET-UP BASE PAGE
    TFR   	 A,DP                                 ;E03F    1F8B         * Set DP to $A000
*
*	CLEAR ALL OF ASCREEN  & variable RAM space
*
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4
    LDD     #$0001
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
		DECB		* Make D = $0000
;		LDD		#$0000  * D is already 0

    LDX      #$0000                		           ;E041    8E0000       * Clear RAM from #$0000 to
!
    STD      ,X++                                 ;E04B    ED81
    STD      ,X++                                 ;E04D    ED81
    CMPX     #$9800                               ;E04F    8CBF80       * Clear to #$97FF  (Screen RAM)
    BLO      <                                    ;E052    25F1         * If RAM from $0000 to $BF7F is not all made $00 then loop
* We need to restore MMU Blocks 0 to 4 banks to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4

* Clearr from $9800 to $B204
;    CLRB                                          ;E044    5F
    LDD     #$0000
_Branch_E045:
;    LDA      #$39            ; Bits: 0011 1001    ;E045    8639
;    STA      Watchdog                             ;E047    B7CBFF       Feed the watchdog
;    CLRA                                          ;E04A    4F
    STD      ,X++                                 ;E04B    ED81
    STD      ,X++                                 ;E04D    ED81
    CMPX     #$B206                               ;E04F    8CBF80       * Clear to #$BF7F
    BLO      _Branch_E045                         ;E052    25F1         * If RAM from $0000 to $BF7F is not all made $00 then loop

*				REG.A & REG.B = 0
    INCB                                          ;E054    5C           * B=1 some make it lower memory ROM mode  #$01 (READ ROM)
    STB      DRRUC								                ;E055    F7BFFF       Variable to flag if we are in ROM or RAM mode 1=ROM mode, 0=RAM mode THIS 1ST BECAUSE OF INTERUPTS USING THIS BIT
    STB      RRUC                									;E058    F7C900       Set ROM or RAM mode 1=ROM mode, 0=RAM mode



*
*  PIA INITIALIZATION
*  DATA DIRECTION
;    STA      IO_Board_PIA_Ctrl_A                  ;E05B    B7C805       I/O borad PIA CTRL_A
;    STA      IO_Board_PIA_Ctrl_B                  ;E05E    B7C807       I/O borad PIA CTRL_B
;    STA      IO_Board_PIA_Data_A                  ;E061    B7C804       I/O borad PIA DATA_A
;    STA      IO_Board_PIA_Data_B                  ;E064    B7C806       I/O borad PIA DATA_B
;    LDB      #$34            ; Bits: 0011 0100    ;E067    C634
;    STB      IO_Board_PIA_Ctrl_A                  ;E069    F7C805       I/O borad PIA CTRL_A
;    STB      IO_Board_PIA_Ctrl_B                  ;E06C    F7C807       I/O borad PIA CTRL_B
*
*	IRQ, 7-SEG, SOUND PIA INITIALIZATION
*	INTERRUPTS
*
;    STA      ROM_PIA_CTRL_A                       ;E06F    B7C80D       PIA Control A setting
;    STA      ROM_PIA_CTRL_B                       ;E072    B7C80F       PIA Control B setting
;    STA      ROM_PIA_DATA_A                       ;E075    B7C80C       PIA Data A reading/writing
;    COMA                                          ;E078    43           * A=$FF
;    STA      ROM_PIA_Sound_LED                    ;E079    B7C80E       PIA Data B reading/writing - bit 0 to 5 = 6 bits to sound board, bits 6 & 7 plus CA2 and CB2 = 4 bits to drive the LED 7 segment
;    LDB      #$34            ; Bits: 0011 0100    ;E07C    C634
;    STB      ROM_PIA_CTRL_A                       ;E07E    F7C80D       PIA Control A setting
;    LDB      #$35            ; Bits: 0011 0101    ;E081    C635
;    STB      ROM_PIA_CTRL_B                       ;E083    F7C80F       PIA Control B setting
;    LDB      #$3F            ; Bits: 0011 1111    ;E086    C63F
;    STB      ROM_PIA_Sound_LED                    ;E088    F7C80E       PIA Data B reading/writing - bit 0 to 5 = 6 bits to sound board, bits 6 & 7 plus CA2 and CB2 = 4 bits to drive the LED 7 segment
*
*	CLOSE COINS SWITCHES VIA SOFTWARE (WHO KNOWS WHAT
*
    LDA      #$3C            ; Bits: 0011 1100    ;E08B    863C
;    STA      <PIA_Input_Related1_A08B            ;E08D    978B         Seems to be related to buttons/coins
    STA      <SWT0							                  ;E08F    97B4					; SWITCH DEBOUNCING STATES, T0,T1,T2,...,T8


*
*	INITILIZE PROCESS LINKS
*
    LDU      #PBLKST                              ;E091    CEA100
    STU      <PFREE						                    ;E094    DF08         * Next Free Link list address ; START LINK OF FREE PROCESS RAM
    LDA      #PBLKM/2        ; Bits: 0001 0100    ;E096    8614         * ALWAYS HAVE AN EVEN NBR OF PROCESSES - Initialize $14=20 entries for this link list (will be from $A100 to $A988 ends at $A9BF)
PLINIT:
    LEAX     PBLKL,U                              ;E098    30C838       * X=U+56  = Next Link List position
    STX      PLINK,U                              ;E09B    AFC4         * Save the X address that U points to
    LEAU     PBLKL,X                              ;E09D    338838       * U = X + 56
    STU      PLINK,X                              ;E0A0    EF84         * Save U @ address X
    DECA                                          ;E0A2    4A           * Decrement our counter
    BNE      PLINIT                               ;E0A3    26F3         * If not all done then loop
		LDX     	#DTBL1 															;E0A5    8EA9C0       START READING DATA FROM DMA BLOCK TABLE #1
    STX     	ITBL             										;E0A8    9F25         Points to either $A9C0 or $AC21 - list of sprites to draw in IRQ
    LDX				#DTBL2                              ;E0AA    8EAC21       Flag to draw sprites Link2 list items - START SECONDARY PROCESS WAITING FOR PREVIOUS
    STX     	PDTBL                 					    ;E0AD    9F68         Points to an address that A uses ($A9C0 was used once) - DMA BLOCK TABLE #2
*
*       INITIAL COLORS
*
;		LDX      #Palette5                            ;E0AF    8EE5FA       Palette Data Table5
		LDX      #COLOR1																									* LOAD DEFAULT COLOR CHART
    JSR      DCOLOR													      ;E0B2    BDE4B2       Update Palette, moves 16 bytes pointed at X to $A00F-$A01E,Kills Y,A,B,X
*
*	INITIAL GAME TIME NBRS
*
		LDD			#60*60                                ;E0B5    CC0E10
		STD			MINUTE      								          ;E0B8    DD99
*
		LDX			#CREDST							                  ;E0BA    8ECD00       CMOS number of credits (max 99)  GET THE CREDITS IN CMOS
;	  JSR      Read_CMOS_BYTES_XINTO_B              ;E0BD    BD3B34       Read 2 CMOS bytes pointed to by X in B and X=X+2
  	JSR			RCMSB																												Read_CMOS_BYTES_XINTO_B
    TFR      B,A                                  ;E0C0    1F98
    CMPA     #$20            ; Bits: 0010 0000    ;E0C2    8120   TOO HI?
    BHI			LSYSTEM001_001                        ;E0C4    2206		BR=YES
    ANDA     #$0F            ; Bits: 0000 1111    ;E0C6    840F		BCD NUMBER?
    CMPA     #$09            ; Bits: 0000 1001    ;E0C8    8109
    BLS	LSYSTEM002_000                   	        ;E0CA    2307		BR=YES
LSYSTEM001_001:
    CLRB                                          ;E0CC    5F
		LDX			#CREDST							                  ;E0CD    8ECD00       CMOS number of credits (max 99) - RESET THE CREDITS IN CMOS
;    JSR      Write_CMOS_B_INTO_X                 ;E0D0    BD3B3D       Write B into CMOS bytes pointed to by X and X=X+2
		JSR			WCMSB
LSYSTEM002_000:
		STB			CREDIT																;E0D3    D7F2         Number of credits also copied to CMOS at 42F5
*
*	START SINGLE PROCESS
*
		LDU			#PDUMMY 															;E0D5    CEA00A				THIS 1ST PROCESS STARTS FROM THE DUMMY PROCESS
		LDX			#VPWRUP		                            ;E0D8    8E5ED0       * Address $5ED0 does a jump to $627F  CREATE POWER-UP MODE PRIMARY PROCESS I.D.=0
    CLRA                                          ;E0DB    4F
    CLRB                                          ;E0DC    5F
		JSR			CUPROC                                ;E0DD    BDE284       Add Link Y=($A008),Y+2=D,Y+4=1,Y+5=X routine address, make U the next free link address in $A008

;    ANDCC    #$EF    ; CC Flags: EFH-NZVC         ;E0E0    1CEF         * Enable the IRQ but not the fast IRQ - EI, LET INTERUPTS FLY!!!

    LDA     #25
    STA     FIRQCount         * 100 FIRQs per frame so each time we count down 25 we will be 1/4 further down the screen
    CLR     VideoBeam        * Video beam just finished drawing the screen and is in VBLANC on it's way to start again at the top so we are now at the top of the screen
    CLR     IRQ_Flag          * Set IRQ 0=not currently doing and IRQ, 1=yes currently doing an IRQ
;    ANDCC     #%11101111      * Enable the IRQ
;    SYNC                      * Wait and do Interrupt to match JOUST hardware
    ANDCC     #%10101111      * Enable FIRQ & IRQ

*
*	EXECUTE THE PROCESSES IN ORDER OF CREATION
*		NOTE; THIS IS NOT A SUBROUTINE
*		      NO STACK OR REGISTERS ARE SAVED
*
EXECST:
	LDX	#DTBL1			PUT NEW IMAGES IN DMA AREA #1?
	LDA	DMAFOR
	BPL	EXTBL1		 	BR=YES
	LDX	#DTBL2			PUT DMA IMAGES IN TABLE 2
EXTBL1	STX	DTBL	REMEMBER FOREGROUND START OF NEXT TABLE
	COM	DMAFOR			NEXT TIME USE THE OTHER DMA BUFFER
EXEINT	TST	,X		IS THIS TABLE READY TO BE WRITTEN INTO?
	BNE	EXEINT		 	BR=NO
	LEAX	DMA1-DTBL1,X	1ST FREE RAM AREA FOR DMA INFO.
	STX	DMAFRE
*
  IF Add_Audio
	LDA	STMR				ANY SOUND LEFT? (Set to 1 in SND: routine when a sound is going to be played, next)
	BEQ	EXESND		 	BR=NO
	DEC	STMR				NEXT TIME
	BNE	EXESND		 	BR=TIME NOT UP YET
	LDX	SNDPTR			GET SOUND GROUP
	BEQ	EXESND		 	BR=NO SOUND LEFT
	LDD	,X++				GET REG.A=SND CODE, REG.B=SND TIME
	BMI	LSYSTEM001_002		CONTINUATION BIT SET
	LDX	#0
LSYSTEM001_002:
	STX	SNDPTR
	STB	STMR
  TFR   A,B         * B = Sound code
  ANDB  #$3F
  LDX   #SampleBlockTable
  LSLB  * B = B * 2
  LSLB  * B = B * 4
  ABX   * X = Table position of our entry, (B=B*4) , 4 bytes per entry in the table
  LDD   ,X++               * A = Block to Use for Sample, B = Loop or not, value 0 = no loop, <> 0 = Yes loop
  STA   MMU_Reg_Bank1_3    * Copy the audio sample block to the location where the audio playback routine will use it (MMU Bank B - 3 $6000-$7FFF)
  STB   DoSoundLoop
  LDX   ,X                 * Get the Sample address
  STX   GetSample+1        * Save the sample start address as the actual sample pointer
  STX   SampleStart        * Also save it if we need to loop the sample
  LDX   #FIRQ_Sound        * Make the FIRQ jump to the sample playing code
  STX   FIRQ_Start_Address * Just in case it's set for non looping and a sample ended

;	LDB	#$FF				SEND THE SOUND TO THE SOUND BOARD
;	STB	PIAB
;	ANDA	#$3F			DO NOT DISTURBE 7-SEGMENT DISPLAY
;	STA	PIAB
EXESND	EQU	*
  ELSE
	LDA	STMR				ANY SOUND LEFT?
	BEQ	EXESND		 	BR=NO
	DEC	STMR				NEXT TIME
	BNE	EXESND		 	BR=TIME NOT UP YET
	LDX	SNDPTR			GET SOUND GROUP
	BEQ	EXESND		 	BR=NO SOUND LEFT
	LDD	,X++				GET REG.A=SND CODE, REG.B=SND TIME
	BMI	LSYSTEM001_002		CONTINUATION BIT SET
	LDX	#0
LSYSTEM001_002:
	STX	SNDPTR
	STB	STMR
	LDB	#$FF				SEND THE SOUND TO THE SOUND BOARD
	STB	PIAB
	ANDA	#$3F			DO NOT DISTURBE 7-SEGMENT DISPLAY
	STA	PIAB
EXESND	EQU	*
  ENDIF
*
	CLRA						NOW RESET FORGROUNDS CLEAR & WRITE POINTERS
	CLRB
	LDX	#DMABEG
	STX	DMAEND-DMABEG,X	1ST LINK OF DMAEND POINTERS
	STD	,X++				ZERO LINK INDICATES NOT IN USE
	STX	DMAEND-DMABEG,X
	STD	,X
	LEAX	QUADB-QUADA-2,X
	STX	DMAEND-DMABEG,X
	STD	,X++
	STX	DMAEND-DMABEG,X
	STD	,X
	LEAX	QUADC-QUADB-2,X
	STX	DMAEND-DMABEG,X
	STD	,X++
	STX	DMAEND-DMABEG,X
	STD	,X
	LEAX	QUADD-QUADC-2,X
	STX	DMAEND-DMABEG,X
	STD	,X++
	STX	DMAEND-DMABEG,X
	STD	,X
*
*	MAIN EXEC LOOP
*
EXEC:
	LDU	#PDUMMY		START AT FIRST PROCESS IN LIST
	LDS	#STACK		STACK IS RESET
	CLR	PRISEC		PRIMARY PROCESSES ARE FIRST
	BRA	EXEPRI
*
EXELOP:
	STU	PEXEC			CURRENTLY EXECUTING PROCESS
	JMP	[PPC,U]		DEFAULT PC SLEEP ADDRESS, GO START/RE-START

NAPTPC:
	LDX	,S++			GET NEXT PC COUNTER
NAPTIM:
	LDU	PEXEC			NAP ENTERANCE (PC=REG.X TIME=REG.A)
	STX	PPC,U			NEW PC COUNTER (REG.X)
	STA	PNAP,U		NEW NAP TIME (REG.A)
EXEPS	LDA	PRISEC		ON PRIMARY OR SECONDARY PROCESSES??
	BNE	EXESEC		BR=SECONDARY
EXEPRI	STU	PPREV		REMEMBER LAST PROCESS EXECUTED
	LDU	PLINK,U		NEXT PROCESS
	BEQ	EXEPEN		BR=NO MORE PRIMARY PROCESSES
	LDA	PPRI,U		IS THIS A PRIMARY PROCESS??
	BNE	EXEPRI		BR=NO, SKIP THIS PROCESS
	DEC	PNAP,U		1 LESS NAP TIME UNIT
	BEQ	EXELOP
	BRA	EXEPRI
*
EXEPEN:
	LDS	#STACK		STACK IS RESET
	COM	PRISEC		START SECONDARY PROCESSES
	LDU	PSEC			START SECONDARY LIST
	BNE	EXESEC
	LDU	#PDUMMY
*
EXESEC
	STU	PPREV			REMEMBER LAST PROCESS EXECUTED
	LDA	[PDTBL]		SECONDARY PROCESS TIME UP?
	BEQ	EXESE2		BR=YES
	LDU	PLINK,U		NEXT PROCESS
	BEQ	EXESEN		BR=NO MORE SECONDARY PROCESSES
	LDA	PPRI,U		IS THIS A SECONDARY PROCESS??
	BEQ	EXESEC		BR=NO, SKIP THIS PROCESS
	DEC	PNAP,U		1 LESS NAP TIME UNIT
	BEQ	EXELOP
	BRA	EXESEC
*
EXESE2	NOP
EXESEN	STU	PSEC		(REMEMBER LAST SECONDARY PROCESS)
*
EXEDON	EQU	*		NOW COMPLETE THE ERASE & WRITE LINKS
*
DTBOVR
	LDU	DTBL			POINT TO CURRENT DMA TABLE
	STU	PDTBL		 	(WILL BE PREVIOUS DATA TABLE)
	LEAU	DTBL1A-DTBL1,U	POINT TO DMA TABLES INTERUPT LINK AREA
	LDX	#0				A ZERO FOR END OF LINK(S)
	LDD	DMABEG+ERASE+QUADA	FOR INTERUPT B, LINK CLEAR A
	STD	,U
	STX	[DMAEND+ERASE+QUADA]	(END OF LINKS)
*
DTBC1
	LDD	DMABEG+ERASE+QUADB	FOR INTERUPT C - CLR B, WR A
	BNE	DTBC2
	LDD	DMABEG+WRITE+QUADA
	STD	2,U
	BRA	DTBC2A
DTBC2
	STD	2,U				CLR B EXISTS, SO LINK IN WR A
	LDD	DMABEG+WRITE+QUADA
	STD	[DMAEND+ERASE+QUADB]
DTBC2A	STX	[DMAEND+WRITE+QUADA]
*
DTBD1	LDD	DMABEG+ERASE+QUADC	FOR INTERUPT D - CLR C, WR B
	BNE	DTBD2
	LDD	DMABEG+WRITE+QUADB
	STD	4,U
	BRA	DTBD2A
DTBD2	STD	4,U		CLR C EXISTS, SO LINK IN WR B
	LDD	DMABEG+WRITE+QUADB
	STD	[DMAEND+ERASE+QUADC]
DTBD2A	STX	[DMAEND+WRITE+QUADB]
*
DTBA1	LDD	DMABEG+ERASE+QUADD	FOR INTERUPT A - CLR D, WR C, WR D
	BNE	DTBA3
	LDD	DMABEG+WRITE+QUADC
	BNE	DTBA2
	LDD	DMABEG+WRITE+QUADD
	STD	6,U
	BRA	DTBA4A
DTBA2	STD	6,U			WR C EXISTS ONLY, SO LINK IN WR D
	LDD	DMABEG+WRITE+QUADD
	STD	[DMAEND+WRITE+QUADC]
	BRA	DTBA4A
DTBA3	STD	6,U			CLR D EXISTS, LINK WR C OR WR D?
	LDD	DMABEG+WRITE+QUADC
	BNE	DTBA4
	LDD	DMABEG+WRITE+QUADD
	STD	[DMAEND+ERASE+QUADD]
	BRA	DTBA4A
DTBA4	STD	[DMAEND+ERASE+QUADD]	CLR D, WR C EXISTS, SO LINK IN WR D
	LDD	DMABEG+WRITE+QUADD
	STD	[DMAEND+WRITE+QUADC]
DTBA4A	STX	[DMAEND+WRITE+QUADD]
*
DTBEN	INC	DTBL1-DTBL1A,U		INDICATE BUFFER READY FOR INTERUPTS
	JMP	EXECST		LOOP FOREVER
*
*	SUCIDE, KILL YOURSELF (EITHER JSR OR JMP)
*
SUCIDE	LDS	#STACK		STACK IS IN THE CLEAR
	LDU	PEXEC		GET EXECUTING PROCESSES BLOCKS
	LDD	PLINK,U		LINK NEXT TASK TO...
	STD	[PPREV]		 PREVIOUSES TASK LINK
	LDD	PFREE		LINK 1ST FREE RAM LINK TO..
	STD	PLINK,U		 THIS CURRENTLY DELETING TASK
	STU	PFREE		LINK DELETEING TASK TO FREE RAM LINK
	LDU	PPREV		MAINTAIN PREVIOUS LINK
	JMP	EXEPS		CONTINUE IN EXEC LOOP
;      IFE DEBUG2
*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE
*
	FCC	'JOUST (C)1982 WILLIAMS ELECTRONICS INC.'
;      ENDIF
*
*	KILL ANY PROCESSES (NOT INCLUDING ITSELF)
*		INPUTS REG.A = PROCESS I.D.
*		       REG.B = MASK FOR MORE THAN 1 I.D.
*
KLPROC	PSHS	D,U
	LDX	#PDUMMY		INIT DUMMY PREVIOUS PROCESS ADDRESS
	LDU	PDUMMY		INIT CURRENT PROCESS AS BEGINNING OF LIST
	BEQ	KLPRTS		 BR=NO PROCESSES TO KILL
KLPLOP	LDB	1,S		GET PROCESS MASK
	ANDB	PID,U		DISCARD UNWANTED BITS
	CMPB	,S		COMPARE WITH PROCESS I.D. TO MATCH
	BNE	KLPNOM		 BR=NO MATCH
	CMPU	PEXEC		IS THIS THE KILLER?
	BNE	KLPKIL		 BR=NO, GO KILL IT
	STX	PPREV		UPDATE PREVIOUSLY EXECUTED PROC(IN CASE OF KIL)
KLPNOM	TFR	U,X		CURRENT PROCESS IS NOW PREVIOUS PROCESS
KLPNXT	LDU	PLINK,U		GETTING NEW CURRENT PROCESS
	BNE	KLPLOP
KLPRTS	PULS	D,U,PC
*
KLPKIL	CMPU	PSEC		IS THE SECONDARY EXECUTION TASK LIST HERE?
	BNE	KLPSEC		 BR=NO
	STX	PSEC		YES, REMEMBER PREVIOUS ACTIVE TASK ADDRESS
KLPSEC	LDD	PLINK,U		LINK NEXT TASK TO...
	STD	PLINK,X		 PREVIOUSES TASK LINK
	LDD	PFREE		LINK 1ST FREE RAM LINK TO..
	STD	PLINK,U		 THIS CURRENTLY DELETING TASK
	STU	PFREE		LINK DELETEING TASK TO FREE RAM LINK
	TFR	X,U		GET BACK INTO ACTIVE TASK LIST
	BRA	KLPNXT		THE OLD PROCESS IS STILL THE OLD PROCESS
*
*
*	CREATE A PROCESS AFTER CURRENT PROCESS
*		INPUT REG.X = START ADDRESS
*		      REG.A = PROCESS I.D.
*		      REG.B = PRIMARY (0) OR SECONDARY (<>0) SELECTION
*		      PEXEC = CURRENT PROCESS BEING EXECUTED
*		OUTPUT REG.Y = NEW PROCESSES BLOCK ADDRESS
*		       REG.U = CURRENT PROCESS BEING EXECUTED
*
CRPROC	LDU	PEXEC
*
*
*	CREATE A PROCESS AFTER REG.U PROCESS
*		INPUT REG.X = START ADDRESS
*		      REG.A = PROCESS I.D.
*		      REG.B = PRIMARY (0) OR SECONDARY (<>0) SELECTION
*		      REG.U = CURRENT PROCESS BLOCK, NEW PROCESS WILL BE NEXT
*		OUTPUT REG.Y = NEW PROCESSES BLOCK ADDRESS
*
CUPROC	PSHS	D
	LDY	PFREE
CRPRAM	LDD	PLINK,Y		REMOVE NEW LINK FROM..
	STD	PFREE		 FREE PROCESS RAM
	LDD	PLINK,U		MAKE NEXT ACTIVE PROC LINK AFTER...
	STD	PLINK,Y		 NEWLY CREATED PROCESS
	STY	PLINK,U		ACTIVATE LINK TO NEWLY CREATE PROCESS
	LDA	#1		NEW - WAKE-UP IMMEDIATELY
	STA	PNAP,Y
	STX	PPC,Y		NEW - PC LOCATION
	PULS	D
	STA	PID,Y		NEW - PROCESS I.D.
	STB	PPRI,Y		NEW - PRIMARY/SECONDARY PROCESS SELECTION
	RTS
*
*	ALLOCATE DMA AREA FOR ERASE OPERATION
*	 INPUT REG.B = LOWEST REG.Y LOCATION
*	 OUTPUT REG.X = DMA BLOCK AREA
*
CL1ALL	ANDB	#$C0		CALC QUADRANT'S POINTER ADDRESS
	LDX	#DMAEND+ERASE
	ABX
	LDD	DMAFRE		GET SOME FREE RAM FOR THIS DMA OPERATION
	STD	[,X]
	ADDD	#8
	STD	,X
	ADDD	#2
	LDX	DMAFRE
	STD	DMAFRE		1 LESS SPACE IN FREE DMA RAM
	RTS
*
*	ALLOCATE DMA AREA FOR WRITE OPERATION
*	 INPUT REG.B = LOWEST REG.Y LOCATION
*	 OUTPUT REG.X = DMA BLOCK AREA
*		CLSX,CLSY = SCREEN OFFSETS
*
WR1ALL	ANDB	#$C0		CALC QUADRANT'S POINTER ADDRESS
	LDX	#DMAEND+WRITE
	ABX
	LDD	DMAFRE		GET SOME FREE RAM FOR THIS DMA OPERATION
	STD	[,X]
	ADDD	#8
	STD	,X
	ADDD	#2
	LDX	DMAFRE
	STD	DMAFRE		1 LESS SPACE IN FREE DMA RAM
	RTS
*
*	ALLOCATE & FLAVOR AREA FOR 1 ERASE OPERATION
*	 INPUT REG.U = PROCESS AREA OF OBJECT
*	 OUTPUT REG.X = DMA BLOCK AREA
*		CLSX,CLSY = SCREEN OFFSETS
*
CL1CLS	LDB	PPOSY+1,U
	STB	CLSY		NEW CLASIFIED Y CO-ORDINATE
	ANDB	#$C0		CALC QUADRANT'S POINTER ADDRESS
	LDX	#DMAEND+ERASE
	ABX
	LDD	DMAFRE		GET SOME FREE RAM FOR THIS DMA OPERATION
	STD	[,X]
	ADDD	#8
	STD	,X
	ADDD	#2
	LDX	DMAFRE
	STD	DMAFRE		1 LESS SPACE IN FREE DMA RAM
	LDD	PPOSX,U
	RORA
	RORB
	STB	CLSX		NEW SCREEN X LOCATION
	LDA	#$1A		ASSUME SER/BLOCK, CONSTANT, & ZERO TRANSFER
	BCC	CL1NOF
	ORA	#$20		FLAVOR THE IMAGE
CL1NOF	CLRB			ZERO CONSTANT BYTE
	STD	,X		DEFAULT DMA COMMAND
	RTS
*
*	ALLOCATE & FLAVOR AREA FOR 1 WRITE OPERATION
*	 INPUT REG.U = PROCESS BLOCK WITH VALID PPOSX,PPOSY
*	 OUTPUT REG.X = DMA BLOCK AREA
*		CLSX,CLSY = SCREEN OFFSETS
*
WR1CLS	LDB	PPOSY+1,U
	STB	CLSY		NEW CLASIFIED Y CO-ORDINATE
	ANDB	#$C0		CALC QUADRANT'S POINTER ADDRESS
	LDX	#DMAEND+WRITE
	ABX
	LDD	DMAFRE		GET SOME FREE RAM FOR THIS DMA OPERATION
	STD	[,X]
	ADDD	#8
	STD	,X
	ADDD	#2
	LDX	DMAFRE
	STD	DMAFRE		1 LESS SPACE IN FREE DMA RAM
	LDD	PPOSX,U
	RORA
	RORB
	STB	CLSX		NEW SCREEN X LOCATION
	LDA	#$0A		ASSUME SERIAL TO BLOCK, ZERO SURPRESS TRANSFER
	BCC	WR1NOF
	ORA	#$20		FLAVOR THE IMAGE
WR1NOF	STA	,X		DEFAULT DMA COMMAND
	RTS
*
*	ALLOCATE & FLAVOR AREA FOR 2 ERASE OPERATION
*	 INPUT REG.U = PROCESS BLOCK WITH VALID PPOSX,PPOSY
*	 OUTPUT REG.X = DMA BLOCK AREA
*		CLSX,CLSY = SCREEN OFFSETS
*
CL2CLS	LDB	PPOSY+1,U
	STB	CLSY		NEW CLASIFIED Y CO-ORDINATE
	ANDB	#$C0		CALC QUADRANT'S POINTER ADDRESS
	LDX	#DMAEND+ERASE
	ABX
	LDD	DMAFRE		GET SOME FREE RAM FOR THIS DMA OPERATION
	STD	[,X]
	ADDD	#8
	STD	,X
	ADDD	#2
	STD	[,X]
	ADDD	#8
	STD	,X
	ADDD	#2
	LDX	DMAFRE
	STD	DMAFRE		1 LESS SPACE IN FREE DMA RAM
	LDD	PPOSX,U
	RORA
	RORB
	STB	CLSX		NEW SCREEN X LOCATION
	LDA	#$1A		ASSUME SER/BLOCK, CONSTANT, & ZERO TRANSFER
	BCC	CL2NOF
	ORA	#$20		FLAVOR THE IMAGE
CL2NOF	CLRB			ZERO CONSTANT BYTE
	STD	,X		DEFAULT DMA COMMAND
	STD	10,X		 IN BOTH AREA'S
	RTS
*
*	ALLOCATE & FLAVOR AREA FOR 2 WRITE OPERATION
*	 INPUT REG.U = PROCESS BLOCK WITH VALID PPOSX,PPOSY
*	 OUTPUT REG.X = DMA BLOCK AREA
*		CLSX,CLSY = SCREEN OFFSETS
*
WR2CLS	LDB	PPOSY+1,U
	STB	CLSY		NEW CLASIFIED Y CO-ORDINATE
	ANDB	#$C0		CALC QUADRANT'S POINTER ADDRESS
	LDX	#DMAEND+WRITE
	ABX
	LDD	DMAFRE		GET SOME FREE RAM FOR THIS DMA OPERATION
	STD	[,X]
	ADDD	#8
	STD	,X
	ADDD	#2
	STD	[,X]
	ADDD	#8
	STD	,X
	ADDD	#2
	LDX	DMAFRE
	STD	DMAFRE		1 LESS SPACE IN FREE DMA RAM
	LDD	PPOSX,U
	RORA
	RORB
	STB	CLSX		NEW SCREEN X LOCATION
	LDA	#$0A		ASSUME SERIAL TO BLOCK, ZERO SURPRESS TRANSFER
	BCC	WR2NOF
	ORA	#$20		FLAVOR THE IMAGE
WR2NOF	STA	,X		DEFAULT DMA COMMAND
	STA	10,X		 IN BOTH AREAS
	RTS

IRQSaveMMU:
* Backup the graphics screens
  LDD   MMU_Reg_Bank0_0
  STD   IRQRestoreMMUs0_1+1
  LDD   MMU_Reg_Bank0_2
  STD   IRQRestoreMMUs2_3+1
  LDA   MMU_Reg_Bank0_4
  LDB   MMU_Reg_Bank0_6
  STD   IRQRestoreMMUs4_6+1
* Set it to normal
* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    LDD			#$3C00+RegRAM6	 * A=$3C, B=Memory block $3E
    STA     MMU_Reg_Bank0_4  * Set Bank 4
		STB			MMU_Reg_Bank0_6	 * Page $C000-$DFFF  Bank #6
    RTS

*
*
*	INTERUPT HANDLER
*

*****************************************
* FIRQ and IRQ stuff
* Tested with MAME, 100 times the FIRQ was triggered between each VSYNC IRQ
* MAME scanline delay count before the scanline is really at 00 (I think??  From CoCo 3 Mame info...)
* Triggered 100 times between each IRQ, which delays it 18 times
*****************************************
VSyncIRQ:
        LDA     IRQENR         	  * Re enable the VSYNC IRQ
        LDA     #VSYNC_Count      * Might want to increase this number since the video beam is moving from the bottom of the screen
                                  * Could give us a little more time until the line is really at row 0, (maybe it would match Joust hardware better?)
                                  * If there is flickering of the sprites maybe this should be adjusted...
        STA     FIRQCount         * 100 FIRQs per frame so each time we count down 25 we will be 1/4 further down the screen
        CLR     VideoBeam         * Video beam just finished drawing the screen and is in VBLANC on it's way to start again at the top
                                  * actually gives us 13 samples until we are really at the top and drawing the screen again but it won't be drawing the screen
                                  * so we will use it as 0, to give the game a little more time to update the screen sprites

;IRQ_Jump:
;IRQ:																			;E38D                 *** Address for IRQ ***


;	STS	IRQSTK		REMEMBER STACK POINTER (NO DCON FROM NOW ON!)
;	LDA	#WDATA
;	STA	WDOG		TICKLE WATCH DOG
	INC	RANDOM
	DEC	RANDOM+1
	LDA	DRRUC		DMA READS ROM!
	ORA	#1
	STA	RRUC
;	LDB	PIAB		CLEAR 4MS
;	LDB	VSCAN

;    LDB      <VideoBeam                            ;E3A4    F6CB00       Current scanline position
;	ANDB	#$C0		SHOW ONLY VALID INTERUPT BITS     ; #$C0 = %1100 0000
;	LBNE	IRQ2		UPDATE COLOR RAM ON VERTICAL LINE 0

*
*	UPDATE COLOR RAM DURING VERTICAL RE-TRACE (REALLY TOP OF SCREEN)
*
;	LDU	#RAMCOL		COLOR RAM REFRESH (DURING VERTICAL RETRACE)
;	PULU	D,X,Y,S
;	LDU	#CRAM+8
;	PSHU	D,X,Y,S
;	LDU	#RAMCOL+8
;	PULU	D,X,Y,S
;	LDU	#CRAM+16
;	PSHU	D,X,Y,S
    opt     c,ct,cc       * show cycle count, add the counts, clear the current count
																																			* COLOR RAM REFRESH (DURING VERTICAL RETRACE)
    LDU      #RAMCOL			                        ;E3AD    CEA00F       Colour slot 0 of the palette * Since scanline is at zero then a new frame is being drawn so Update the palette from $A00F to $A01E
    PULU     A,B,X,Y                              ;E3B0    3776
    LDU      #Palette_Start+6                     ;E3B2    CEC008
    PSHU     A,B,X,Y                              ;E3B5    3676
    LDU      #RAMCOL+6                            ;E3B7    CEA017       Colour slot 1 of the palette
    PULU     A,B,X,Y                              ;E3BA    3776
    LDU      #Palette_Start+12                    ;E3BC    CEC010
    PSHU     A,B,X,Y                              ;E3BF    3676
    LDX      RAMCOL+$0C						* PaletteSlot12_A01B
    STX      Palette_12
    LDX      RAMCOL+$0E						* PaletteSlot14_A01D
    STX      Palette_14

;	LDS	IRQSTK		RESTORE STACK POINTER

*
*	GAME TIME CALCULATIONS
*
	LDA	GOVER		GAME OVER?
	BPL	LSYSTEM001_005		 BR=YES
	LDD	MINUTE		CALC NBR OF MINUTES IN AN ACTIVE GAME
	SUBD	#1
	BGT	LSYSTEM011_000
	LDB	#6		INC BOOKS GAME TIME
	JSR	AUDIT1
	LDD	#60*60		NBR OF INTERUPTS UNTIL 1 MINUTE
LSYSTEM011_000:
	STD	MINUTE
LSYSTEM001_005:

* Simulate WPIAA bits according to keypresses
* WPIAA  Bits5=1 player start,4=2 player start,3=NotUsed,2=P1_Flap,1=P1_JoyRight,0=P1_JoyLeft
* Checkout routine READEM also:
*       PLAYER 1 - READ JOYSTICK
*       PLAYER 2 - READ JOYSTICK
*       Results in value in CURJOY

* WPIAA  Bits5=1 player start,4=2 player start,3=NotUsed,2=P1_Flap,1=P1_JoyRight,0=P1_JoyLeft  $C804 - A INPUT		WIDGET-BRD INPUTS;
* PIAA   Bits7=HighScore_Reset,6=Tilt,5=CoinSlot2,4=CoinSlot1,3=NotUsed,2=CoinSlot3,1=Advance_Button,0=AutoUp/Man Down $C80C - COIN DOOR,INTERRUPT,SOUND BRD PIA'S

* Scan keyboard
* CoCo Keyboard mapping
* Read keyboard  CoCo Keyboard mapping
*Row                Data
*bits   7    6    5    4    3    2    1    0
*0      G    F    E    D    C    B    A    @
*1      O    N    M    L    K    J    I    H
*2      W    V    U    T    S    R    Q    P
*3     SPC  RGT  LFT   DN   UP   Z    Y    X
*4      '    &    %    $    #    "    !    0
*4      7    6    5    4    3    2    1
*5      ?    >    =    <    +    *    )    (
*5      /    .    _    ,    ;    :    9    8
*6   SHIFTS  F2   F1  CTRL ALT  BRK  CLR  ENT
***********************************************************
        opt        cc
        LDX     #PIA0_Byte_0_KeyRow_Joy             * PIA0
        CLRB                * Start with no bits on
  IF SaveCMOS
***********************************************************************************
* ESC/BREAK on keyboard for Advance button (Column 2, Row 6)
*            bits 76543210
!       LDA     #%11111011  * Store Column bit 2
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
*            bits 76543210
        BITA    #%01000000  * Test Row bit 6
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00000010  * otherwise we set bit 1  = (Advance button pressed)
  ENDIF
***********************************************************************************
* 5 on keyboard for coin insert in the center slot (Column 5, Row 4)
*            bits 76543210
!       LDA     #%11011111  * Store Column bit 5
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
*            bits 76543210
        BITA    #%00010000  * Test Row bit 4
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00100000  * otherwise we set bit 5  = (coin inserted, 5 pressed)
***********************************************************************************
!
        STB     PIAA        * Save B's value in the Button_Other (PIAA) memory location
        CLRB                * Start with no bits on
***********************************************************************************
* 1 on keyboard for one player game start (Column 1, Row 4)
*            bits 76543210
!       LDA     #%11111101  * Store Column bit 1
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
*            bits 76543210
        BITA    #%00010000  * Test Row bit 4
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00100000  * otherwise we set bit 5 of B = 1 Player Start in Joust
***********************************************************************************
* 2 on keyboard for two player game start (Column 2, Row 4)
*            bits 76543210
!       LDA     #%11111011  * Store Column bit 2
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
*            bits 76543210
        BITA    #%00010000  * Test Row bit 4
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00010000  * otherwise we set bit 4 of B = 2 Player Start in Joust
***********************************************************************************
!       TST     KeyboardOrJoystick * 0 = Keyboard, <> 0 = Joystick
        BNE     GetJoy      * Go read joystick controls instead of Keyboard (no conflicts)
***********************************************************************************
* C on keyboard for P1 Flap (Column 3, Row 0)
P1FColumn:
*            bits 76543210
        LDA     #%11110111  * Store Column bit 3
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
P1FRow:
*            bits 76543210
        BITA    #%00000001  * Test Row bit 0
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00000100  * Otherwise we set bit 2 of B = P1 Flap in Joust
***********************************************************************************
* S on keyboard for P1 Right (Column 3, Row 2)
P1RColumn:
*            bits 76543210
!       LDA     #%11110111  * Store Column bit 3
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
P1RRow:
*            bits 76543210
        BITA    #%00000100  * Test Row bit 2
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00000010  * Otherwise we set bit 1 of B = P1 Right in Joust
***********************************************************************************
* A on keyboard for P1 Left (Column 1, Row 0)
P1LColumn:
*            bits 76543210
!       LDA     #%11111101  * Store Column bit 1
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
P1LRow:
*            bits 76543210
        BITA    #%00000001  * Test Row bit 0
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00000001  * Otherwise we set bit 0 of B = P1 Left in Joust
***********************************************************************************
!       STB     WPIAA_P1       * Save B's value in the Button_Other (WPIA) memory location

* Get Player 2 controls
        CLRB                * Start with no bits on
***********************************************************************************
* / on keyboard for P2 Flap (Column 7, Row 5)
P2FColumn:
*            bits 76543210
        LDA     #%01111111  * Store Column bit 7
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
P2FRow:
*            bits 76543210
        BITA    #%00100000  * Test Row bit 5
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00000100  * Otherwise we set bit 2 of B = P2 Flap in Joust
***********************************************************************************
* L on keyboard for P2 Right (Column 4, Row 1)
P2RColumn:
*            bits 76543210
!       LDA     #%11101111  * Store Column bit 4
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
P2RRow:
*            bits 76543210
        BITA    #%00000010  * Test Row bit 1
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00000010  * Otherwise we set bit 1 of B = P2 Right in Joust
***********************************************************************************
* K on keyboard for P2 Left (Column 3, Row 1)
P2LColumn:
*            bits 76543210
!       LDA     #%11110111  * Store Column bit 3
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
P2LRow:
*            bits 76543210
        BITA    #%00000010  * Test Row bit 1
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00000001  * Otherwise we set bit 0 of B = P2 Left in Joust
***********************************************************************************
!       STB     WPIAA_P2    * Save B's value for player 2 controls
        JMP     DoneControls * Skip Joystick controls, we only want keyboard

* We already have some bits in B for WPIAA_P1, so don't CLRB here
GetJoy:
***********************************************************************************
* Use any column you want (so it doesn't interfere with your keyboard buttons)
* Right Joystick button 1 output is 1111 1110 (Red top button on Deluxe Joystick)
* Left  Joystick button 1 output is 1111 1101 (Red top button on Deluxe Joystick)
* Right Joystick button 2 output is 1111 1011 (Black button on Deluxe Joystick)
* Left  Joystick button 2 output is 1111 0111 (Black button on Deluxe Joystick)
*            bits 76543210
!       LDA     #%11111110  * Store Column bit 0 (@,H,P,X,0,(,8,Enter - these cannot be used)
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
*            bits 76543210
        BITA    #%00000010  * Test Row bit 1 (Button 1 for Left Joystick)  No switch is 1111 1111, switch pressed is 1111 1101
        BNE     >           * Skip ahead if set
        ORB     #%00000100  * otherwise we set bit 2 = P1 Flap
!       BITA    #%00001000  * Test Row bit 3 (Button 2 for Left Joystick)  No switch is 1111 1111, switch pressed is 1111 0111
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00000100  * otherwise we set bit 2 = P1 Flap
***********************************************************************************
!       STB     WPIAA_P1       * Save B's value in the Button_Other (WPIA) memory location
        CLRB
***********************************************************************************  Right 1 is both, right 2 is only right
* Use any column you want (so it doesn't interfere with your keyboard buttons)
* Right Joystick button 1 output is 1111 1110 (Red top button on Deluxe Joystick)
* Left  Joystick button 1 output is 1111 1101 (Red top button on Deluxe Joystick)
* Right Joystick button 2 output is 1111 1011 (Black button on Deluxe Joystick)
* Left  Joystick button 2 output is 1111 0111 (Black button on Deluxe Joystick)
*            bits 76543210
!       LDA     #%11111110  * Store Column bit 0 (@,H,P,X,0,(,8,Enter - these cannot be used)
        STA     $02,X       * in $FF02
        LDA     ,X          * Read keyboard $FF00
*            bits 76543210
        BITA    #%00000001  * Test Row bit 0 (Button 1 for Right Joystick)  No switch is 1111 1111, switch pressed is 1111 1110
        BNE     >           * Skip ahead if set
        ORB     #%00000100  * otherwise we set bit 2 = P2 Flap
!       BITA    #%00000100  * Test Row bit 2 (Button 2 for Right Joystick)  No switch is 1111 1111, switch pressed is 1111 1011
        BNE     >           * Skip ahead if set
*            bits 76543210
        ORB     #%00000100  * otherwise we set bit 2 = P2 Flap
***********************************************************************************
!       STB     WPIAA_P2    * Save B's value for player 2 controls

* Joystick support
* Mux values bit 3 on PIA 0 Side A = $FF01 and Side B = $FF03
* $FF03   $FF01
* MSb 0 & LSb 0  = Joystick 0 (Right) Horizontal input
* MSb 1 & LSb 0  = Joystick 1  (Left) Horizontal input
***********************************************************
        opt     c,ct,cc       * show cycle count, add the counts, clear the current count
;        LDX     #PIA0_Byte_0_KeyRow_Joy	* $FF00   * X is already set to this value
        LDU     #PIA1_Byte_0_IRQ				* $FF20
        LDA     #%00110100              * clear CB2 output (disable audio)
        STA     PIA1_Byte_3_IRQ_Ct_Snd	* $FF23
        ORCC    #%01010000              * Disable interrupts so we don't mess this up when the sound/timer IRQ fires
        LDB     PIA1_Byte_0_IRQ         * save the original 6-bit audio output value   $FF20

* Right joystick Horizontal value (Right/Left movement) input
Joystick0_X:
				LDA			#$34										* A = $34 which disables the VSYNC IRQ?  Proably don't need this, since we use FIRQ VSYNC on the CoCo 3
        STA     PIA0_Byte_3_VSYNC				* $FF03   - A = $34 = 0011 0100 Bit 3 is the MSb of the MUX line, value sets it to 0
        STA     PIA0_Byte_1_HSYNC				* $FF01   - A = $34 = 0011 0100 Bit 3 is the LSb of the MUX line, value sets it to 0
* Above just selected MSb 0 & LSb 0 of Mux which selects Right joystick Horizontal value (Right left movement) input
				LDA			#$67
        STA     ,U											* $FF20   -  A = $67 test for low value on selected axis = 0110 0111 6 bit range is 01 1001 = $19 = 25
        NOP															* Wait to make sure the value is registered
        NOP															* ""
        TST     ,X											* Check value in $FF00
        BPL     Joystick0_X_is_Left			* If bit 7 is a 0 then it is lower then this value, jump to set Joystick is in the left position
        LDA     #$9A                    * test for high value on selected axis = 1001 1010 6 bit range is 10 0110 = $26 = 38
        STA     ,U											* $FF20   - Store test for 38
        NOP															* Wait to make sure the value is registered
        NOP															* ""
        TST     ,X											* Check value in $FF00
        BPL     Joystick0_X_is_Middle		* If bit 7 is a 0 then it is lower then this value so skip to save as middle position, otherwise
																				* Fall through and set it as the Right position
Joystick0_X_is_Right:
        LDA     #%00000010              * X is in Right position - Joust Bit 1
        ORA     WPIAA_P2
        STA     WPIAA_P2
        BRA     DoneJoystick0_X
Joystick0_X_is_Left:
        LDA     #%00000001              * X is in the Left position - Joust Bit 0
        ORA     WPIAA_P2
        STA     WPIAA_P2
Joystick0_X_is_Middle:
DoneJoystick0_X:

* Left joystick Horizontal value (Right/Left movement) input
Joystick1_X:
				LDA			#$3C
        STA     PIA0_Byte_3_VSYNC				* $FF03   - A = $3C = 0011 1100 Bit 3 is the MSb of the MUX line, value sets it to 1
				LDA			#$34
        STA     PIA0_Byte_1_HSYNC				* $FF01   - A = $34 = 0011 0100 Bit 3 is the LSb of the MUX line, value sets it to 0
* Above just selected MSb 1 & LSb 0 of Mux which selects Left joystick Horizontal value (Right left movement) input
				LDA			#$67
        STA     ,U											* $FF20   -  A = $67 test for low value on selected axis = 0110 0111 6 bit range is 01 1001 = $19 = 25
        NOP															* Wait to make sure the value is registered
        NOP															* ""
        TST     ,X											* Check value in $FF00
        BPL     Joystick1_X_is_Left			* If bit 7 is a 0 then it is lower then this value, jump to set Joystick is in the left position
        LDA     #$9A                    * test for high value on selected axis = 1001 1010 6 bit range is 10 0110 = $26 = 38
        STA     ,U											* $FF20   - Store test for 38
        NOP															* Wait to make sure the value is registered
        NOP															* ""
        TST     ,X											* Check value in $FF00
        BPL     Joystick1_X_is_Middle		* If bit 7 is a 0 then it is lower then this value so skip to save as middle position, otherwise
																				* Fall through and set it as the Right position
Joystick1_X_is_Right:
        LDA     #%00000010              * X is in Right position - Joust Bit 1
        ORA     WPIAA_P1
        STA     WPIAA_P1
        BRA     DoneJoystick1_X
Joystick1_X_is_Left:
        LDA     #%00000001              * X is in the Left position - Joust Bit 0
        ORA     WPIAA_P1
        STA     WPIAA_P1
Joystick1_X_is_Middle:
DoneJoystick1_X:

* Exit Joystick reading routine we can now put the
        LDA     #%00110100
        STA     PIA0_Byte_1_HSYNC       * HSYNC IRQ Disabled, IRQ Polarity Flag falling Edge, Data Direction Normal, Select Line LSb = 0, HSYNC Flag = 0
        STA     PIA0_Byte_3_VSYNC       * VSYNC IRQ Disabled, IRQ Polarity Flag falling Edge, Data Direction Normal, Select Line MSb = 0, VSYNC Flag = 0
        STB     PIA1_Byte_0_IRQ         * restore the previous DAC value (for audio)
        LDA     #%00111100              * DAC6 mode: re-enable audio output
        STA     PIA1_Byte_3_IRQ_Ct_Snd	* $FF23
        ANDCC   #%10111111							* Re enable the FIRQ

* We are now finished reading the players controls
DoneControls:


*
*	SWITCH DEBOUNCER, USED FOR A COMMON GAME.
*	THIS ROUTINE IS SERVICED EVERY 1/60 SEC AND 2 CONSECTIVE
*	CLOSURES MEANS THE SWITCH IS CLOSED, THEN 9 CONSECTIVE OPENS
*	MEANS THE SWITCH IS OPEN
*
	LDA	SWSLAM		DECREMENT SLAM SWITCH TIMER DEBOUNCE
	BEQ	IRQSLM		 BR=SLAM NEVER CLOSED
	DEC	SWSLAM		 1 LESS TIME UNIT TO GO.
IRQSLM
	LDD	SWT0+6		DEBOUNCE SWITCHES, MOVE SWITCH TIME TABLE
	STD	SWT0+7
	LDD	SWT0+4
	STD	SWT0+5
	LDD	SWT0+2
	STD	SWT0+3
	LDD	SWT0
	STD	SWT0+1
	LDA	PIAA		READ SWITCHES (3 COINS,H.S. RESET,ADVANCE)
	STA	SWT0+0
	BITA	#$40		SLAM SWITCH CLOSED?
	BEQ	IRQNSL		 BR=NO
	LDB	#2*60		2 SECOND DELAY BEFORE COIN ACCEPTANCE
	STB	SWSLAM
IRQNSL
	LDB	SWT0+0		DEBOUNCE SO THAT 3 STABLE SAMPLES IS THE
	ORB	SWT0+1		 DEBOUNCED STATE
	ORB	SWT0+2
	ORB	SWT0+3
	ORB	SWT0+4
	ORB	SWT0+5
	ORB	SWT0+6
	ORB	SWT0+7
	ORB	SWT0+8		THE MINIMIZED STATE TABLE IS
	ANDB	SWDEB		 DEBounced switch
	STB	SWTEMP		  = T0*T1 + DEB*(T0+T1+T2+T3+T4+T5+T6+T7+T8)
	ANDA	SWT0+1
	ORA	SWTEMP
	LDB	SWDEB		REMEMBER OLD DEBOUNCED STATE FOR 0 TO 1 CHANGE
	STA	SWDEB
	COMB			NOW, SHOW SWITCHES THAT JUST TURNED ON
	ANDB	SWDEB
	ANDB	#$3E		REVIEL DEBOUNCED SWITCHES TO ACT UPON
	BEQ	IRQNSW		 BR=NO SWITCH PRESSED
	LSRB
	PSHS	B
	LSR	,S		IS THIS SWITCH ACTIVE?
	BCC	IRNADV		 BR=NO
  JSR   IRQSaveMMU    * Go backup the graphics screen, just in case we are showing demo graphics
  IF SaveCMOS
    JMP   Exit_to_BASIC  * Save settings in format that BASIC can read and save to disk quickly
  ELSE
	  JSR	ADVANC		ADVANCE BUTTON PRESSED
  ENDIF
IRNADV	LDA	SWSLAM		SLAM SWITCH CLOSED RECIENTLY?
	BNE	IRNRCO		 BR=YES, SKIP COIN SWITCHES
	LSR	,S		IS THIS SWITCH ACTIVE?
	BCC	IRNRCO		 BR=NO
  JSR   IRQSaveMMU    * Go backup the graphics screen, just in case we are showing demo graphics
	JSR	COINRV		RIGHT COIN SLOT
IRNRCO	LSR	,S		IS THIS SWITCH ACTIVE?
	BCC	IRNHSR		 BR=NO
  JSR   IRQSaveMMU    * Go backup the graphics screen, just in case we are showing demo graphics
	JSR	RESHSB		H.S. RESET BUTTON PRESSED
IRNHSR	LDA	SWSLAM		SLAM SWITCH CLOSED RECIENTLY?
	BNE	IRNCCO		 BR=YES, SKIP COIN SWITCHES
	LSR	,S		IS THIS SWITCH ACTIVE?
	BCC	IRNLCO		 BR=NO
  JSR   IRQSaveMMU    * Go backup the graphics screen, just in case we are showing demo graphics
	JSR	COINLV		LEFT COIN SLOT
IRNLCO	LSR	,S		IS THIS SWITCH ACTIVE?
	BCC	IRNCCO		 BR=NO
  JSR   IRQSaveMMU    * Go backup the graphics screen, just in case we are showing demo graphics
	JSR	COINCV		CENTER COIN SLOT
* Put graphics screen back to the way it was before we checked the buttons
IRQRestoreMMUs0_1:
  LDD   #$FFFF    * Self mod will change this
  STD   MMU_Reg_Bank0_0
IRQRestoreMMUs2_3:
  LDD   #$FFFF    * Self mod will change this
  STD   MMU_Reg_Bank0_2
IRQRestoreMMUs4_6:
  LDD   #$FFFF    * Self mod will change this
  STA   MMU_Reg_Bank0_4
  STB   MMU_Reg_Bank0_6
IRNCCO	PULS	B
IRQNSW	EQU	*		END OF SWITCH DEBOUNCER

*
*	BEAM INTERFERENCE FOR BUFFERED DMA I/O OPERATIONS
*
IRQ2:

  COM     IRQ_Flag          * Set the IRQ flag as active

  LDX	ITBL		POINT TO SELECTED DMA TBLE BLOCK
	LDA	ISQCNT			REMEMBER LAST QUADRANT WRITTEN OUT
	TST	,X					WHAT MODE IS CURRENT INTERUPT DMA TABLE IN?
	LBEQ	IRQRTS			BR=FOREGROUND NOT READY WITH DATA
	BMI	IRQNXT		 	BR=ALREADY STARTED DMA'ING THIS TABLE
	COM	,X+					NEW STARTING IMAGE, & POINT TO FIRST LINK
	STX	ISQPTR			REMEMBER THIS INTERUPT QUADRANT TO UPDATE
	CLRA						1ST QUADRANT TO BE UPDATED
*
*	WARNING; SENSITIVE INTERUPT DRIVEN DMA OPERATIONS
*		DMA OPERATION COULD HAVE DESTROYED REG.DP
*
IRQNXT	STA	>ISQCNT
;	EORA	>VSCAN		ABLE TO WRITE DATA IN THIS QUADRANT WITHOUT
	EORA	VideoBeam
	ANDA	#$C0		 		BEAM INTERFERANCE?
	LBEQ		IRQRTS			BR=BEAM INTREFERANCE, DO NOT WRITE DATA!
	LDX		>ISQPTR			NEXT QUADRANT TO UPDATE ON SCREEN
	LDU		,X++
	STX		>ISQPTR
	LEAX	,U					ANY DATA TO MOVE?
	LBEQ		IRQSKD1		 	BR=NO, SO SKIP THE DMA OPERATION
;	DEC	>CPIAB			DISABLE PIA INTERUPTS (PLAYING WITH CC)
;	LDA	PIAB				CLEAR 4MS INTERUPT, JUST IN CASE...

* Drawing Sprites with stack blasting the Blitter

* Set lower RAM to screen mode
    LDD     MMU_Reg_Bank0_0  	* Get Banks 0 & 1 - Graphics RAM banks - otherwise when cliffs disappear game will crash...
    STD     IRQRestoreBank01+1 * Self modify code below
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     MMU_Reg_Bank0_2  	* Get Banks 2 & 3 - Graphics RAM banks
    STD     IRQRestoreBank23+1
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDA     MMU_Reg_Bank0_4   * Get Bank 4 value
    STA     IRQRestoreBank4+1 * Self modify code below
    INCB
    STB     MMU_Reg_Bank0_4   * Set Bank 4 value
    LDB     MMU_Reg_Bank0_6   * Get the current Bank 6 value
    STB     IRQRestoreBank6+1 * Store in selfmod code below
IRQDMA:
;	LDS		#DMA+8				START/RESTART DMA LOOPING
;	PULU	CC,D,DP,X,Y

  PULU  A,B,X,Y
;If A=$1A then writing a block to the screen
; colour is in B,X can be ignored, TFR Y,D - D has Joust screen destination,  LDD ,U++  A & B Xor #$04 A = width of block, B=Height

;=12 =   K,W A=88 - Block copy
;=1A = K,Z,W A=00 - Block copy It cleared some memory, if A has a value then it used if the destination is not zero
;=0A =   Z,W A=00 - 2C6C,64BF - Copied sprite to screen any zeros in the source are transparent
;=2A =   Z,W A=00 - 2C6C,64BF - Copied sprite to screen shifted one pixel to the right any zeros in the source are transparent
; 3A =   Draw sprite but fill it in with color in A

	IF UseCompiledSprites
  BITA  #%01000000        * Test if it's a transporter related sprite
  LBNE  WriteSprite_Trans * If not zero then this is a transporter sprite go handle it
  ENDIF

  BITA  #$10        * Is it a solid colour? block copy to do? = $1A or $12?
  BEQ   WriteSprite * Skip ahead if not
* Do a block copy of pattern B
  JSR     BlastRectangle
	LDU		,U						NEXT DMA OPERATION
	BNE		IRQDMA		 		BR=VALID LINK
  JMP   IRQSKD        Done all the sprites/blocks

;IF A=$0A then copying a sprite to the screen
; B can be ignored
; X has the source address (Probably can use the compiled sprite version here to make it quicker)
; TFR Y,D - D has Joust screen destination,  LDD ,U++  A & B Xor #$04 A = width of sprite, B=Height
WriteSpriteShifted:

EndofScreen  EQU  $A0
EndofScreenNew EQU  EndofScreen-1

	IF UseCompiledSprites
    CMPA    #$2A
    LBNE    ObjectInLava
    TFR     Y,D         * D = Y
    CMPA    #EndofScreen  * Check if the X co-ordiante is higher then 160 (160 bytes across the screen)
    BLO     >           * If lower then ok to draw, skip ahead
  IF TestScreenEdge
    LDA     #EndofScreenNew
    DECB
  ELSE
    LEAU    2,U         * Otherwise don't draw it, move U to the next entry
    BRA     DoneWritingSprite  * skip over writing sprite code
  ENDIF
!
    STA     WriteSpriteShifted1+2      * Save A in selfmod ADDD below
    LDA     #160                * D = B * 160 (Y co-ordinate * 160)
    MUL
WriteSpriteShifted1:
    ADDD    #$0000              * Self modified from above
    TFR     D,Y                 * Y now has proper CoCo3 screen location
; If we are using Compiled sprites then X points to the address that points to the code that will draw the sprite we can use a JSR [,X]
    LDA     #$38                * Bank 0
    CMPX  #$2000                * Is the pointer less then the first block?
    BLO   >
    INCA                        * Bank 1
    LEAX  -$2000,X              * Reduce the pointer by $2000
!
    STA   MMU_Reg_Bank0_6     * Set Bank 6 - so we can get the address of the sprite drawing routine
    LEAX  $C000,X             * X = X +$C000 so it points to the proper location
    LDA   2,X                 * Get the block that has the code to draw this sprite
    LDX   ,X                  * X now points to the actual code to be executed
    STA   MMU_Reg_Bank0_6     * Set Bank 6
    ORCC  #%00000001          * Set the carry bit so it will draw a sprite shifted to the right (draw the Odd sprite)
    JSR   ,X                  * Jump to our compiled sprite routine
    LEAU  2,U                 * Move U to the start of the next sprite data info
    BRA   DoneWritingSprite   * Jump forward
  ELSE
  CMPA    #$2A
  BNE     WriteSpriteShifted   * Endless loop if we get here - need to handle a different Blitter value
* Draw sprite on screen shifted to the right - Skip for now
  LDA     #$0A
  ENDIF

WriteSprite:
  CMPA    #$0A    * Copy a sprite the way it is?
  BNE     WriteSpriteShifted        * If not it might need to be shifted to the right one pixel
  TFR     Y,D         * D = Y
  CMPA    #EndofScreen  * Check if the X co-ordiante is higher then 160 (160 bytes across the screen)
  BLO     >           * If lower then ok to draw, skip ahead
  IF TestScreenEdge
    LDA     #EndofScreenNew
    DECB
  ELSE
    LEAU    2,U         * Otherwise don't draw it, move U to the next entry
    BRA     DoneWritingSprite  * skip over writing sprite code
  ENDIF
!
  STA     WriteSprite1+2      * Save A in selfmod ADDD below
  LDA     #160                * D = B * 160 (Y co-ordinate * 160)
  MUL
WriteSprite1:
  ADDD    #$0000              * Self modified from above
	IF UseCompiledSprites
    TFR     D,Y                 * Y now has proper CoCo3 screen location
; If we are using Compiled sprites then X points to the address that points to the code that will draw the sprite we can use a JSR [,X]
  LDA     #$38                * Bank 0
    CMPX  #$2000                * Is the pointer less then the first block?
    BLO   >
    INCA                        * Bank 1
    LEAX  -$2000,X              * Reduce the pointer by $2000
!
    STA   MMU_Reg_Bank0_6     * Set Bank 6 - so we can get the address of the sprite drawing routine
    LEAX  $C000,X             * X = X +$C000 so it points to the proper location
    LDA   2,X                 * Get the block that has the code to draw this sprite
    LDX   ,X                  * X now points to the actual code to be executed
    STA   MMU_Reg_Bank0_6     * Set Bank 6
    JSR   ,X                  * Jump to our compiled sprite routine
    LEAU  2,U                 * Move U to the start of the next sprite data info
  ELSE

  TFR     D,Y                 * Y now has proper CoCo3 screen location
* Sprite data is in blocks $38 & $39, $38 = address 0000-$1FFF & $39 is $2000-#3FFF
* Since we need the low RAM to be graphics we can move the source blocks to $C000-$DFFF and point to the source there

  LDA     #$38                * Bank 0
  LEAX    $C000,X             * X=X+$C000 so if the sprite source data is in $0000-$1FFF area we are good
  CMPX    #$E000
  BLO     >
  INCA                        * Bank 1
  LEAX    -$2000,X            * If not then it was $2000-$3FFF so reduce it another $2000 so it's in the range of $C000-$DFFF
!
  STA     MMU_Reg_Bank0_6     * Set Bank 6
  EXG     X,Y                 * X now has the screen address and Y has the source data address
  LDD     ,U++                * A = Width and B = the height of the block to copy, move U to the next pointer location
  EORA    #$04                * Fix Width
  STA     WriteSprite2+1      * Save it for later
  NEGA
  ADDA    #160                * A = -Width+160 = start of next row
  STA     WriteSprite2+2      * Save it for later
  LDA     WriteSprite2+1      * Get width again
  EORB    #$04                * Fix Height
  STB     BlockHeight     * Save the height
  BRA     >               * Skip over moving X for the first row
* Draw block A wide & B High
WriteSprite2:
  LDD     #$0000          * Self modified from above A = width, B=width of a row =160 bytes - width of block to create
  ABX                     * Move X to the start of the next row
!
  LDB     ,Y+
  STB     ,X+
  DECA
  BNE     <
  DEC     BlockHeight
  BNE     WriteSprite2

  ENDIF

DoneWritingSprite:
	LDU		,U						NEXT DMA OPERATION
	LBNE		IRQDMA		 		BR=VALID LINK
;   DMA EQUATES     *
;A  DMA     EQU     $CA00   CONTROL REGISTER OF DMA CC
;B  CON     EQU     DMA+1   CONSTANT WRITE REGISTER A
;X  ORG     EQU     CON+1   ORIGIN (WHERE DATA IS FROM) 2-BYTES B+DP
;Y  DES     EQU     ORG+2   DESTINATION (WHERE DATA IS TO GO) 2-BYTES X
;A  XSIZE   EQU     DES+2   HORIZONTAL SIZE (X-SIZE) Y
;B  YSIZE   EQU     XSIZE+1 VERTICAL SIZE (Y-SIZE)   y
;DMA BITS: H,L,F,K Z,S,W,R
;          E,F,H,I N,.,V,C
;                        R-READ BLOCK
;                      W-WRITE BLOCK
;                    S-SLOW FOR RAM
;                  Z-SUPRESS ZEROES
;                K-REPLACE DATA WITH CONSTANT
;              F-FLAVOR RIGHT
;            L-SUPRESS CONSTANT LOW NIBBLE
;          H-SUPRESS CONSTANT HIGH NIBBLE
;=12 =   K,W A=88 - Block copy
;=1A = K,Z,W A=00 - Block copy It cleared some memory, if A has a value then it used if the destination is not zero
;=0A =   Z,W A=00 - 2C6C,64BF - Copied sprite to screen any zeros in the source are transparent
;=2A =   Z,W A=00 - 2C6C,64BF - Copied sprite to screen shifted one pixel to the right any zeros in the source are transparent
; 3A =   Draw sprite but fill it in with color in A

;	PSHS	CC,D,DP,X,Y		DMA OPERATION STARTED & COMPLETED
;	LDU		,U						NEXT DMA OPERATION
;	BNE		IRQDMA		 		BR=VALID LINK
;	ORCC	#$FF					DMA COMPLETE, MAKE SURE INTERUPTS ARE OFF!
;	INC		>CPIAB				ENABLE PIA INTERUPTS TO MICRO-PROCESSOR
IRQSKD:
* Put blocks back to normal
IRQRestoreBank01:
    LDD     #$FFFF           * will be self modified above as we enter main Sprite routine
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
IRQRestoreBank23:
    LDD     #$FFFF           * will be self modified above as we enter main Sprite routine
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
IRQRestoreBank4:
		LDB     #$FF             * Self modified from above
    STB     MMU_Reg_Bank0_4  * Set Bank 4
IRQRestoreBank6:
		LDB     #$FF             * Self modified from above
    STB     MMU_Reg_Bank0_6  * Set Bank 6

IRQSKD1:
	LDA		>ISQCNT			BUMP QUADRANT COUNTER (SAME AS VERT COUNTER)
	ADDA	#$40
	LBNE		IRQNXT			BR=MORE QUADRANTS TO DO, TRY TO DO IT ANY WAYS
	LDX		>ITBL				FREE UP THE DMA BUFFER
	CLR		,X
	LDX		#DTBL1			NEXT DMA BUFFER UPDATES SCREEN
	COM		>DMAINT
	BPL		IRQBUF		 	BR=DMA TABLE 1
	LDX		#DTBL2			HERE IF DMA TABLE 2
IRQBUF:
	STX		>ITBL		REMEMBER THIS TABLE
IRQRTS:
;	LDS	>IRQSTK
	LDA		>DRRUC		RESTORE READ REGISTER!
	STA		>RRUC
;	RTI

  CLR     IRQ_Flag          * We are done the IRQ so flag it as done
  RTI


	IF UseCompiledSprites
* This code will draw a red rectangle at the bottom of the sprite that it just drew to make it look like it's sinking in Lava
* in Joust routine CLPIT: we will use WCCON which is B value after the PULS as the number of rows down from the original sprites top row
* then we can calculate where the rectangle should be drawn.  We will also not write a new height in the CLPIT routine so we have the
* info we need at ,U
ObjectInLava:
  CMPA    #$8A    * Copy a sprite the way it is?  Bit 7 is now set since it's flaged as in lava
  BNE     ObjectInLavaShifted        * If not it might need to be shifted to the right one pixel

* Y has the starting Joust screen location of the sprite
* Save it as self mod code below
  STY     ObjectInLava2+2

* B at this point has the starting Y loction of the rectangle
* Save it as self mod code below so rectangle will now have the correct starting location
  STB     ObjectInLava2+3

  TFR     Y,D         * D = Y
  CMPA    #EndofScreen  * Check if the X co-ordiante is higher then 160 (160 bytes across the screen)
  BLO     >           * If lower then ok to draw, skip ahead
  IF TestScreenEdge
    LDA     #EndofScreenNew
    DECB
  ELSE
    LEAU    2,U         * Otherwise don't draw it, move U to the next entry
    BRA     DoneWritingSprite  * skip over writing sprite code
  ENDIF
!
  STA     ObjectInLava1+2     * Save A in selfmod ADDD below
  LDA     #160                * D = B * 160 (Y co-ordinate * 160)
  MUL
ObjectInLava1:
  ADDD    #$0000              * Self modified from above
    TFR     D,Y               * Y now has proper CoCo3 screen location
; If we are using Compiled sprites then X points to the address that points to the code that will draw the sprite we can use a JSR [,X]
  LDA     #$38                * Bank 0
    CMPX  #$2000              * Is the pointer less then the first block?
    BLO   >
    INCA                      * Bank 1
    LEAX  -$2000,X            * Reduce the pointer by $2000
!
    STA   MMU_Reg_Bank0_6     * Set Bank 6 - so we can get the address of the sprite drawing routine
    LEAX  $C000,X             * X = X +$C000 so it points to the proper location
    LDA   2,X                 * Get the block that has the code to draw this sprite
    LDX   ,X                  * X now points to the actual code to be executed
    STA   MMU_Reg_Bank0_6     * Set Bank 6
    JSR   ,X                  * Jump to our compiled sprite routine

* Now we need to draw a box of lava to cover the sprite
* To blast a rectangle on the screen at the proper location we need:
* A = Block description:  Not needed we know it's a block
* B = Colour of the block we know it's LAVA = $44
* X = Source address and can be ignored
* Y = Destination address
* U points to the width & height of the block (Exclusive OR'd with $04, for the width and then again for the Height)
* This will be changed to the jump address below

    LDB   #$44                * Palette value for LAVA
ObjectInLava2:
    LDY   #$FFFF              * Self mod code from above it will first have the original sprite starting point then the height value (LSB) has
                              * just been written
    JSR   BlastRectangle      * Go draw the Lava coloured rectangle overtop the bottom of the sprite, just like poor Gollum "My, Precious!"
    JMP   DoneWritingSprite   * Jump

ObjectInLavaShifted:
* Y has the starting Joust screen location of the sprite
* Save it as self mod code below
  STY     ObjectInLavaShifted2+2

* B at this point has the starting Y loction of the rectangle
* Save it as self mod code below so rectangle will now have the correct starting location
  STB     ObjectInLavaShifted2+3
    CMPA    #$AA
    BNE     ObjectInLava  * Endless loop if there's something else happenning
    TFR     Y,D         * D = Y
    CMPA    #EndofScreen  * Check if the X co-ordiante is higher then 160 (160 bytes across the screen)
    BLO     >           * If lower then ok to draw, skip ahead
  IF TestScreenEdge
    LDA     #EndofScreenNew
    DECB
  ELSE
    LEAU    2,U         * Otherwise don't draw it, move U to the next entry
    JMP     DoneWritingSprite  * skip over writing sprite code
  ENDIF
!
    STA     ObjectInLavaShifted1+2      * Save A in selfmod ADDD below
    LDA     #160                * D = B * 160 (Y co-ordinate * 160)
    MUL
ObjectInLavaShifted1:
    ADDD    #$0000              * Self modified from above
    TFR     D,Y                 * Y now has proper CoCo3 screen location
; If we are using Compiled sprites then X points to the address that points to the code that will draw the sprite we can use a JSR [,X]
    LDA     #$38                * Bank 0
    CMPX  #$2000                * Is the pointer less then the first block?
    BLO   >
    INCA                        * Bank 1
    LEAX  -$2000,X              * Reduce the pointer by $2000
!
    STA   MMU_Reg_Bank0_6     * Set Bank 6 - so we can get the address of the sprite drawing routine
    LEAX  $C000,X             * X = X +$C000 so it points to the proper location
    LDA   2,X                 * Get the block that has the code to draw this sprite
    LDX   ,X                  * X now points to the actual code to be executed
    STA   MMU_Reg_Bank0_6     * Set Bank 6
    ORCC  #%00000001          * Set the carry bit so it will draw a sprite shifted to the right (draw the Odd sprite)
    JSR   ,X                  * Jump to our compiled sprite routine

    LDB   #$44                * Palette value for LAVA
ObjectInLavaShifted2:
    LDY   #$FFFF              * Self mod code from above it will first have the original sprite starting point then the height value (LSB) has
                              * just been written
    JSR   BlastRectangle      * Go draw the Lava coloured rectangle overtop the bottom of the sprite, just like poor Gollum "My, Precious!"
    JMP   DoneWritingSprite   * Jump
  ENDIF

*
*	LOAD 16 COLOR BYTES FROM [REG.X] INTO RAM COPY
*	 (ONLY REFRESH COLOR I/O PORT RAM DURING VERTICAL RETRACE)
*
DCOLOR:
	LDY	#RAMCOL		LOAD COPY OF COLOR RAM
DCOLR2:
	LDB	#16		16 COLORS
DCLRLP:
	LDA	,X+
	STA	,Y+
	DECB
	BNE	DCLRLP
	RTS
*
*	SIMPLE RANDOM NUMBER GENERATOR
*	 OUTPUT; REG.A - A RANDOM NUMBER 0 TO 127
*		 CARRY BIT - RANDOM BIT 0 OR 1
*	 DESTROIES; NONE (EXCEPT REG.A)
*
RAND:
	LDA		RANDOM+1	RANDOM BIT (IN CARRY)
	LSRA
	LSRA
	LSRA
	EORA	RANDOM+1
	LSRA
	ROR		RANDOM
	ROR		RANDOM+1
	RTS
*
*	SIMPLE SOUND PROCESSOR
*	 INPUT; REG.X = ADDRESS OF SOUND TABLE
*	 OUTPUT; TOP MOST SOUND QUE'D & SENT AT BEGINNING OF NEXT FRAME
*	 DESTROIES; REG.D
*
SND:
	LDA		,X+		GET SOUND PIRORITY
	BMI		LSYSTEM001_006		SOUND PIRORITYS OF 128 TO 255 ARE ALWAYS SENT
	LDB		GOVER		NO SOUNDS 0 TO 127 DURING END OF GAME
	BPL		NOSND
LSYSTEM001_006:
	LDB		STMR		SOUND STILL SOUNDING
	BEQ		SN1NS		 BR=NO SOUND
	CMPA	SPRI		OK TO INTERUPT THIS PIRORITY SOUND?
	BLO		NOSND		 BR=NO
SN1NS:
	STA		SPRI		NEW PIRORITY
	LDA		#1		ALLOW FOR NEXT SOUND
	STA		STMR
	STX		SNDPTR		SAVE SOUND TABLE ADDRESS
NOSND:
	RTS

*************************************************************************
*	LAVA EFFECTS BY KEN LANTZ					*
*									*
*	TO BE STARTED AT BEGINNING OF GAME & CONTINUES TILL E.O.G.	*
*	to be in high memory for screen access				*
*	ASSUMES SAFRAM TO BE $EA THEN $E5 THEN $E0			*
*************************************************************************
CLR3	EQU	$4	RED LAVA COLOR EQUATE
LN	EQU	16	nybble moving constants left nybble = nybble *16
RN	EQU	1	right nybble = nybble *1
BN	EQU	17	both nybbles = nybble *17   (hex equevelent of 11)
*****************************************************************************
LAVA:
  PSHS  Y     * Save Y since the original didin't touch it
	LDX	#$00FF		start at the bottom left edge
********	LDA	#$20		init-a-nap (used later)
********	STA	PFRAME-1,U
	STX	SAFRM2		which we will refer to and update
	LDD	#LXPOST		INIT LAVA X POSITION SELECTOR
	STD	LXPOS2
NEXTLN:
	PCNAP	20		Sleep long enough for the cliffs to be in place
	LDX	SAFRM2		get the line and location

  LEAY  ,X    * Save the X value in Y even though it is SAFRM2
  TFR X,D     D=X
* Change Joust Screen location to CoCo3 value in D
    STA     SelfModLava1+2
    LDA     #160              * 160 bytes per row
    MUL
SelfModLava1:
    ADDD    #$0000
    TFR     D,X     X now has the CoCo3 Left starting address on screen

	LDA	DRRUC		LOOK AT THE SCREEN
	ANDA	#$FE				;#!N$01
	STA	DRRUC		THIS 1ST BECAUSE OF INTERUPTS USING THIS BIT
	STA	RRUC

* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    INCB
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank

	LDB	#CLR3*BN	lava color

LSYSTEM001_007:
	STB    ,X+     ; STB	,X
;	LEAX	$100,X
  LEAY  $100,Y   Just to keep track of pointer easily
	 CMPY  #$9600      ; CMPX	#$9600		MAXIMUM SCREEN ADDRESS?
	BHI	SPECIAL		 BR=YES
	LDA	,X	      *	is place in where ever
	ANDA	#$EE    * Is it part of the Cliff?
	BEQ	LSYSTEM001_007		BR = we don't have cliff, Loop until we hit the cliff
*
SP1	BITA	#$F0		here we may have a pixel left over
	BNE	LSYSTEM002_001		 but then we must do the right side lava level
	ORA	#CLR3*LN
	STA	,X         * Save the nibble with the right nibble part of the cliff
LSYSTEM002_001:
  LDD	SAFRM2
	LDA	#$96		same loop but moving in from the right edge
;	TFR	D,X

  TFR D,Y   D=Y = Joust screen location to come in from the right side
;  TFR X,D
* Change Joust Screen location to CoCo3 value in D
    STA     SelfModLava2+2
    LDA     #160              * 160 bytes per row
    MUL
SelfModLava2:
    ADDD    #$0000
    TFR     D,X     X now has the CoCo3 Right starting address on screen
	LDB	#CLR3*BN
LSYSTEM001_008:
  STB	,X
  LEAX  -1,X
	LDA	,X
	BEQ	LSYSTEM001_008
*
SP2	BITA	#$0F		do we have a left over nybble?
	BNE	SPECIAL
	ORA	#CLR3*RN
	STA	,X
SPECIAL:
	LDA	DRRUC		!we are done with the screen so turn
	ORA	#$01		 every thing back on
	STA	DRRUC		THIS 1ST BECAUSE OF INTERUPTS USING THIS BIT
	STA	RRUC

* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
		INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4

	DEC	SAFRM2+1	up one line
CHECK:
	LDB	SAFRM2+1
	CMPB	SAFRAM
	LBGE	NEXTLN
  PULS  Y

  IF LavaBubble
*
* If lava is up to level process falls on thru to do the bubbles
*
	PCNAP	1
	JSR	VRAND
	STA	PFRAME-1,U	pseudo random bubble location
	ANDA	#$0F
	LDX	LXPOS2
	LDA	A,X
	BCC	BURSTR
*
*	DO A "BUBBLE" OR A BUBBLE THAT IS IN MIDDLE OF THE LAVA
*
  JSR MyBubble
	BRA	CHECK
*
*	DO A "BURSTER" OR A BUBBLE THAT IS ON THE SURFACE OF THE LAVA
*
BURSTR:
  JSR MyBuster
	BRA	CHECK
  ELSE
*
* If lava is up to level process falls on thru to do the bubbles
*
	PCNAP	1
	JSR	VRAND
	STA	PFRAME-1,U	pseudo random bubble location
	ANDA	#$0F
	LDX	LXPOS2
	LDA	A,X
	BCC	BURSTR
*
*	DO A "BUBBLE" OR A BUBBLE THAT IS IN MIDDLE OF THE LAVA
*
	LDB	PFRAME-2,U
	ANDB	#$0F		limits bubbles to range E2 to EF
	ORB	#$02		so that they stay in the lava
	ADDB	SAFRM2+1	USE INTERNAL LIMITS, NOT SAFRAM
	INCB
	TFR	D,X
	STX	PFRAME+4,U
	LDA	#CLR3*LN
;	STA	,X
	LDX	#SNBUB		THE SOUND A BUBBLE MAKES
	JSR	SND
	PCNAP	8
	LDX	PFRAME+4,U
	LDA	#CLR3*RN	$03
;	STA	,X
;	STA	$100,X
	LDA	#CLR3*LN	$30
;	STA	-1,X
;	STA	1,X
	PCNAP	8
	LDX	PFRAME+4,U
	LDA	#CLR3*BN	$33
;	STA	-1,X
;	STA	,X
;	STA	1,X
;	STA	$100,X
	LDA	#5
	ADDA	PFRAME-2,U	This rolls the height of the bubbles
	STA	PFRAME-2,U
	BRA	CHECK
*
*	DO A "BURSTER" OR A BUBBLE THAT IS ON THE SURFACE OF THE LAVA
*
BURSTR	LDB	SAFRM2+1
	INCB
	STD	PFRAME+4,U
	TFR	D,X
;	CLR	1,X
	PCNAP	8
	LDX	PFRAME+4,U
	LDB	#CLR3*BN
;	STB	1,X
;	CLR	,X
;	STB	-1,X
	PCNAP	8
	LDX	PFRAME+4,U
	LDB	#CLR3*BN
;	STB	,X
;	STB	-$101,X
;	STB	$FF,X
;	CLR	-1,X
	PCNAP	8
	LDX	PFRAME+4,U
;	CLR	-$101,X
;	CLR	$FF,X
	JMP	CHECK
  ENDIF
*
LXPOST	FCB	$1C,$17,$10,$0D,$06,$04,$74,$78
	FCB	$7F,$85,$89,$8C,$15,$90,$83,$05
	FCB	$AC
*
*	SCREEN COLORS
*
COLOR1:
* Used for colours of normal game play (at least the platform colours are correct)
Palette5:                                             ;E5FA                 Palette Data Table6
;         CoCo  Joust
    FCB    $00  ;$00                                  ;E5FA COLOR 0 - BACKGROUND COLOR
    FCB    $3F  ;$FF                                  ;E5FB
    FCB    $13  ;$70                                  ;E5FC COLOR 2 -
    FCB    $03  ;$58                                  ;E5FD
    FCB    $24  ;$0F                                  ;E5FE	COLOR 4 - MAN'S BODY
    FCB    $36  ;$3F                                  ;E5FF
    FCB    $03  ;$51                                  ;E600	COLOR 6 -
    FCB    $19  ;$E8                                  ;E601
    FCB    $22  ;$14                                  ;E602	COLOR 8 -
    FCB    $0A  ;$90                                  ;E603
    FCB    $23  ;$5D                                  ;E604	COLOR 10-
    FCB    $02  ;$11                                  ;E605
    FCB    $26  ;$1F                                  ;E606	COLOR 12-
    FCB    $38  ;$A4                                  ;E607
    FCB    $04  ;$0A                                  ;E608	COLOR 14-
    FCB    $35  ;$67                                  ;E609
;	FCB	@000	@000	COLOR 0 - BACKGROUND COLOR
;	FCB	@377	@377
;	FCB	@160	@071	COLOR 2 -
;	FCB	@130	@007
;	FCB	@017	@017	COLOR 4 - MAN'S BODY
;	FCB	@077	@077
;	FCB	@121	@111	COLOR 6 -
;	FCB	@350	@350
;	FCB	@024	@024	COLOR 8 -
;	FCB	@220	@220
;	FCB	@135	@062	COLOR 10-
;	FCB	@021	@145
;	FCB	@037	@121	COLOR 12-
;	FCB	@244	@244
;	FCB	@012	@012	COLOR 14-
;	FCB	@147	@147
*
*	ROUTINE TO DRIVE THE 1 COMPACTED IMAGE
*
NEWCL5:
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDD     #$0400+Cliff5_And_Scoring
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
    STB     MMU_Reg_Bank0_6  	* Set Bank 6 to use our Drawing code for Cliff5

    JSR     Draw_Bottom_Platform    Go draw bottom platform / CLIFF5

* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
		LDD     #$3C3E
    STA     MMU_Reg_Bank0_4  * Set Bank 4
    STB     MMU_Reg_Bank0_6  	* Set Bank 6 to normal
    RTS

* Draw Lava Bridge
DrawLavaBridge:
    PSHS  X     * Save X
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDD     #$0400+Cliff5_And_Scoring
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
    STB     MMU_Reg_Bank0_6  	* Set Bank 6 to use our Drawing code for Cliff5
* Draw Left Ground line over lava
    LDX     #$00+$D3*160+160  * Starting location = Joust address = $00D3 , CoCo3 starts a row down since we draw three rows at a time
    LDA     #$88              * Colour
    LDB     #$1B              * Width
!   STA     -160,X
    STA     160,X
    STA     ,X+
    DECB
    BNE     <
* Draw Right Ground line over lava
    LDX     #$78+$D3*160+160  * Starting location = Joust address = $78D3 , CoCo3 starts a row down since we draw three rows at a time
    LDA     #$88              * Colour
    LDB     #$1F              * Width
!   STA     -160,X
    STA     160,X
    STA     ,X+
    DECB
    BNE     <
* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
		LDD     #$3C3E
    STA     MMU_Reg_Bank0_4  * Set Bank 4
    STB     MMU_Reg_Bank0_6  	* Set Bank 6 to normal
    PULS    X,PC      Restore X and return

* somebody commented out the rest of this section, could be used for more of my code

;	LDX	#$1B*2		START X PIXEL POSITION ON SCREEN
;	LDY	#$D3		START Y PIXEL POSITION ON SCREEN
;	LDU	VCOMCL5_SHRAM		START OF COMPACTED DATA
*				FALL INTO UN-COMPACTING ROUTINE
*
*	UN-COMPACTING ROUTINE
*	 INPUT;	REG.X = X PIXEL POSITION
*		REG.Y = Y PIXEL POSITION
*		REG.U = COMPACTED IMAGE START ADDRESS
*
;UNCOM	PSHS	D,X,Y		SAVE SOME REGGIES
;	CLR	,S		RESET COMPACT BIT MONITER
;UNCLP	LDB	,S		GET LAST COMPACTED BITS OF
;	CLRA			RESET LENGTH CODE
;LSYSTEM010_000:	INCA			1 MORE ENCODED LENGTH
;	ASLB			GET NEXT BIT
;	BNE	LSYSTEM015_000		BR=BYTE NOT EMPTY YET
;	LDB	,U+		NEXT COMPRESSED BYTE
;	ASLB			NEXT COMPRESSED BIT
;	INCB			SET BYTE EMPTY FLAG
;LSYSTEM015_000:	BCC	LSYSTEM010_000		 BR=NOT AT END OF LENGTH
;	BSR	REST		GET THE RESET OF THE CODEWORD IN REG.A
;	SUBA	#2		CODEWORD & RUN LENGTH ARE TWO OFF
;	STA	1,S		SAVE RUN LENGTH
;	LDA	#3		GET 3 BIT COLOR ??NIBBLE??
;	BSR	REST
;	STB	,S		SAVE THESE COMPACTED BITS
;	LDB	1,S		GET SAVED RUN LENGTH
;	BNE	LSYSTEM030_000		 BR=1 OR MORE RUN LENGTH (A VALID RUN LENGTH)
;	ANDA	#$07		"END OF LINE" OR "END OF IMAGE"?
;	BEQ	LSYSTEM025_000		 BR=EOI
;	LDX	2,S		RELOAD BEGINNING OF PIXEL LINE
;	LEAY	1,Y		NEXT PIXEL LINE
;	BRA	UNCLP		DECODE NEXT LINE
;*
;LSYSTEM025_000:	PULS	D,X,Y,PC	RETURN TO CALLER
;*
;LSYSTEM030_000:	ANDA	#$07		UNCOMAPCT 3 BIT COLOR NIBBLE
;	BEQ	LSYSTEM033_000
;	ADDA	#8-1
;LSYSTEM033_000:	EQU	*
;*
;*	WRITE PIXEL ROUTINE (NEEDS TO READ SCREEN)
;*	INPUT;	REG.A = LOWER NIBBLE OF COLOR
;*		REG.X = X PIXEL POSITION
;*		REG.Y = Y PIXEL POSITION
;*	OUPUT;	REG.X = REG.X+1
;*		PIXEL ON THE SCREEN
;*
;WRPIX	PSHS	D,Y		SAVE REGGIES AGAIN
;	LDA	DRRUC		LOOK AT THE SCREEN
;	ANDA	#$FE				;#!N$01
;	STA	DRRUC		THIS 1ST BECAUSE OF INTERUPTS USING THIS BIT
;	STA	RRUC
;LSYSTEM033_001:	TFR	X,D		CALC X BYTE LOCATION
;	RORA
;	RORB
;	TFR	B,A
;	LDB	3,S		GET Y BYTE LOCATION
;	TFR	D,Y		CRT ADDRESS FOR THE PIXEL
;	LDA	,S		GET COLOR NIBBLE
;	BCS	LSYSTEM010_001		 BR=LOWER NIBBLE (ODD X PIXEL ADDRESS)
;	ASLA
;	ASLA
;	ASLA
;	ASLA
;LSYSTEM010_001:
;	ORA	,Y		ASSUMES A BLANK SCREEN
;	STA	,Y
;	LEAX	1,X		NEXT PIXEL LOCATION
;	DEC	1,S			ANY MORE RUNS LEFT FOR THIS LENGTH?
;	BNE	LSYSTEM033_001		 BR=YES
;	LDA	DRRUC		LOOK AT THE ROM
;	ORA	#$01
;	STA	DRRUC		THIS 1ST BECAUSE OF INTERUPTS USING THIS BIT
;	STA	RRUC
;	PULS	D,Y
;	BRA	UNCLP		DECODE NEXT RUN LENGTH
;*
;*	DECODE THE REST OF THE INFORMATION
;*	 INPUT;	REG.A = NUMBER OF BITS TO FETCH
;*		REG.B = NORMAL COMPACTED BITS LEFT
;*		REG.U = CURRENT ADDRESS OF NEXT BIT
;*	OUPUT;	REG.A = UNCOMPRESSED DATA
;*		REG.B & REG.U = CONTINUATING COMPRESSED STATUS
;*
;REST	PSHS	A		SAVE NBR OF BITS TO GET
;	LDA	#1		RUN LENGTH START BIT
;LSYSTEM010_002:	ASLB			GET NEXT BIT
;	BNE	LSYSTEM015_001		BR=BYTE NOT EMPTY YET
;	LDB	,U+		NEXT COMPRESSED BYTE
;	ASLB			NEXT COMPRESSED BIT
;	INCB			SET BYTE EMPTY FLAG
;LSYSTEM015_001:	ROLA			PUT BIT INTO ANSWER
;	DEC	,S		ANY MORE BITS LEFT?
;	BNE	LSYSTEM010_002		 BR=YES
;	LEAS	1,S		UNSAVE THE BIT LENGTH DATA
;	RTS


*
*	SOUND TABLE FOR THE MODULE
*
SNBUB	FCB	001,$72,1		THE SOUND OF A BUBBLE
*
ENDADR	EQU	*	SHOULDN'T BE GREATER THAN $E697
LENGTH_SYSTEM	EQU	*-SYSV
;	IFGT *-$E697
;	 FCB	$1111	ADDRESS GREATER THAN $E697 ERROR
;	ENDIF
*


;	ORG	$E6A8
*
*	INCREMENT SCORE BY THOUSANDS AND HUNDREDS
*	 INPUT REG.X - DECISION BLOCK
*	       REG.A - THOUSANDS & HUNDREDS BCD DIGIT
*	DESTROIES REG.Y & REG.D
*	SAVES REG.X
*
SCRHUN	LDY	DSCORE,X	GET SCORE AREA
	BEQ	SCRRTS
	ADDA	2,Y
	BRA	SCRCOM
*
*	INCREMENT SCORE BY HUNDREDS AND TENS
*	 INPUT REG.X - DECISION BLOCK
*	       REG.A - TENS & HUNDREDS BCD DIGIT (BACKWARDS)
*	DESTROIES REG.Y & REG.D
*	SAVES REG.X
*
SCRTEN	LDY	DSCORE,X	GET SCORE AREA
	BEQ	SCRRTS
	TFR	A,B
	ANDA	#$F0		SCORE TENS
	ANDB	#$0F			& HUNDREDS
	ADDA	3,Y
	DAA
	STA	3,Y
	TFR	B,A
	ADCA	2,Y
SCRCOM	DAA
	STA	2,Y
	INC	4,Y		SCORE OCCURED
	LDA	1,Y
	ADCA	#0
	DAA
	STA	1,Y
	LDA	,Y
	ADCA	#0
	DAA
	BCC	L001_069
	ADDA	#$10		PEGG TEN MILLIONS FOR ZERO SURPRESSION DETECT
L001_069:	STA	,Y
	LDD	LEVPAS		IF LEVEL PASS =0 THEN NO EXTRA MEN
	BEQ	SCRRTS
SCRLEV	LDD	1,Y		GET 100'S OF THOUSANDS TO 100'S.
	ADDA	7,Y		 ADD OFFSET FOR NO OVERFLOW
	DAA
	CMPD	8,Y		PAST LEVEL FOR EXTRA MAN?
	BLO	SCRRTS		 BR=NO
	LDA	9,Y		FIGURE OUT NEXT LEVEL
	ADDA	LEVPAS+1
	DAA
	STA	9,Y
	LDA	8,Y
	ADCA	LEVPAS
	DAA
	STA	8,Y
	CMPA	#$30		THE MAGIC NUMBER IS 3000XX BEFORE NEW OFFSET
	BLO	SCRPLY		AWARD THE EXTRA PLAYER
	ADDA	#$A0-$20	SUBTRACT  BCD 20
	DAA
	STA	8,Y
	LDA	7,Y		ALSO OFFSET THE SCORE TO REFLECT THE NEW LEVEL
	ADDA	#$A0-$20	SUBTRACT  BCD 20
	DAA
	STA	7,Y
SCRPLY	PSHS	X,Y		SAVE DECISION BLOCK
	JSR	INCLIV		AWARD THE PLAYER
	LDX	#SNREPL		MAKE REPLAY SOUND
	JSR	VSND
	LDB	#5		INCREMENT BOOKS NBR OF FREE MEN
	JSR	AUDIT1
	PULS	X,Y
	BRA	SCRLEV		RE-CHECK FOR LEVEL PASS (LOW LEVELS)
*
SCRRTS	RTS
*
*	SCORE TABLE FOR PLAYER 1
*
SCRPL1	FDB	SPLY1+4			SCORE CHANGED FLAG
	FDB	$1A00+PL1*$11		SCORE COLOR
	FDB	SPLY1+0,$24D9,BCDLSN	1,000,000 DIGIT INFORMATION
	FDB	SPLY1+1,$27D9,BCDMSN	  100,000 DIGIT INFORMATION
	FDB	SPLY1+1,$2AD9,BCDLSN	   10,000 DIGIT INFORMATION
	FDB	SPLY1+2,$2DD9,BCDMSN	    1,000 DIGIT INFORMATION
	FDB	SPLY1+2,$30D9,BCDLSN	      100 DIGIT INFORMATION
	FDB	SPLY1+3,$33D9,BCDMSN	       10 DIGIT INFORMATION
	FCB	0,0,-1			END OF SCORE INFORMATION
SCRINT	FDB	SPLY1+3,$36D9,BCDLSN	        1 DIGIT INFORMATION
	FCB	0,0,-1			END OF SCORE INFORMATION
*
*	SCORE TABLE FOR PLAYER 2
*
SCRPL2	FDB	SPLY2+4			SCORE CHANGED FLAG
	FDB	$1A00+PL2*$11		SCORE COLOR
	FDB	SPLY2+0,$4BD9,BCDLSN	1,000,000 DIGIT INFORMATION
	FDB	SPLY2+1,$4ED9,BCDMSN	  100,000 DIGIT INFORMATION
	FDB	SPLY2+1,$51D9,BCDLSN	   10,000 DIGIT INFORMATION
	FDB	SPLY2+2,$54D9,BCDMSN	    1,000 DIGIT INFORMATION
	FDB	SPLY2+2,$57D9,BCDLSN	      100 DIGIT INFORMATION
	FDB	SPLY2+3,$5AD9,BCDMSN	       10 DIGIT INFORMATION
	FCB	0,0,-1			END OF SCORE INFORMATION
	FDB	SPLY2+3,$5DD9,BCDLSN	        1 DIGIT INFORMATION
	FCB	0,0,-1			END OF SCORE INFORMATION
*
*	BCD DISPLAY OF SCORE (PLAYER 1 & 2)
*	 INPUT: PRAM,U - ADDRESS OF SCORE TABLE
*
SCODSP	LDX	PRAM,U		PUT UP SCORE ZERO
	LEAX	SCRINT-SCRPL1,X
*
L100_003:	STX	PRAM+2,U	REMEMBER DIGIT TO PUT UP
	LDY	PRAM,U		GET DMA CONTROL & COLOR	NIBBLE
	LDU	2,Y
	LDY	2,X		GET DESTINATION ADDRESS
	BMI	L001_070		 BR=END OF LIST
	LDB	[,X]		GET SCORE BYTE
	JSR	[4,X]		CALL OUTPUT ROUTINE & WAIT
	LDX	PRAM+2,U	NEXT DIGIT
	LEAX	6,X
	BRA	L100_003
*
L001_070:	PCNAP	2
	LDX	PRAM,U		SCORE CHANGED?
	LDB	[,X]
	BEQ	L001_070		 BR=NO
	CLR	[,X]		RESET CHANGED SCORE FLAG
	LEAX	4,X
	LDB	[,X]		LEADING ZERO BLANKING
	BNE	L100_003		 BR=MILLIONS TO SCORE
	LEAX	6,X
	LDB	[,X]		100,000 & 10,000 CHECK
	BEQ	L002_035		 BR=CHECK HUNDREDS DIGIT
	ANDB	#$F0
	BNE	L100_003		 BR=100,000
	LEAX	6,X
	BRA	L100_003		HERE IF 10,000
*
L002_035:	LEAX	6+6,X
	LDB	[,X]		1,000 & 100 CHECK
	BEQ	L003_018		BR=NO DIGITS HERE
	ANDB	#$F0
	BNE	L100_003
	LEAX	6,X		100'S TO DISPLAY
	BRA	L100_003
*
L003_018:	LEAX	6+6,X
	BRA	L100_003		10'S TO DISPLAY
*
*	DISPLAY BCD SCORE, (NAPS BETWEEN DIGITS)
*	  REG.B HAS NIBBLE,
*	  REG.Y HAS DESTINATION ADDRESS
*	  REG.U HAS DMA CONTROL & CONSTANT
*
BCDMSN:
	RORB			HIGH BYTE SHIFT
	RORB
	RORB
	RORB
BCDLSN:
	ANDB	#$0F		LOW BYTE SHIFT
;	LDA	#3*7+2		FONT SIZE IS 6X7 PIXELS PLUS 2BYTE SIZE
;	MUL
;	PSHS	CC
;	ORCC	#$F0		NO INTERUPTS FOR DMA (TIME FOR INT TO SETTLE)
;	ADDD	[FONT5]
;	ADDD	#2		SKIP THE SIZE, IT IS 3X7
;	STD	SDMA     * ORIGIN (WHERE DATA IS FROM) 2-BYTES
;	LDD	#$0703			;#$0307!XDMAFIX
;	STD	WDMA     * HORIZONTAL SIZE (X-SIZE),VERTICAL SIZE (Y-SIZE)
;	STY	DDMA     * DESTINATION (WHERE DATA IS TO GO) 2-BYTES
;	LDD	#$12*256+(DKB*$11)	ERASE OLD DIGIT  - Draw Dark brown box
;	STB	KDMA     * CONSTANT WRITE REGISTER
;	STA	DMA      * CONTROL REGISTER OF DMA
;	TFR	U,D      * U=$1A + $55 for yellow for player one (left) and $77 for Blue, background is $EE
;	STB	KDMA		WRITE NEW DIGIT   * CONSTANT WRITE REGISTER
;	STA	DMA      * CONTROL REGISTER OF DMA

	PSHS	CC
	ORCC	#$F0		NO INTERUPTS FOR DMA (TIME FOR INT TO SETTLE)
* Setup the correct block to draw the players scores in the correct colour
    LDA     #Cliff5_And_Scoring
		STA			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table
    JSR     DrawPlayersScore  * Draw score for player on screen
    LDA			#RegRAM6	 * A=Memory block $3E
    STA     MMU_Reg_Bank0_6  * Set Bank 6
	PULS	CC		NAP BEFORE RETURNING TO CALLER
	LDA	#1
	JMP	VNAPTPC		NAP OF 1, BEFORE NEXT DIGIT(S)
*
********	NLIST			DON'T BOTHER WITH DATA LISTS
*
*	LITTLE PLAYERS SITTING ON BOXES, READY TO PLAY
*
SCOPL1	EQU	*	PLAYER 1 IMAGES LEFT (SIZE $0307)
	FCB	$00,PL1*$10+WHI,$00
	FCB	$00,PL1*$11,$00
	FCB	$00,PL1*$10,$00
	FCB	PL1,PL1*$11,$00
	FCB	PL1,PL1*$11,$00
	FCB	$00,PL1*$11,PL1*$10
	FCB	WHI,WHI*$10,PL1*$11
*
SCOPL2	EQU	*	PLAYER 2 IMAGES LEFT (SIZE $0307)
	FCB	$00,YELLOW*$10+PL2,$00
	FCB	$00,PL2*$11,$00
	FCB	$00,PL2,$00
	FCB	$00,PL2*$11,PL2*$10
	FCB	$00,PL2*$11,PL2*$10
	FCB	PL2,PL2*$11,$00
	FCB	PL2*$11,WHI,WHI*$10
*
*	BACKGROUND BOX COLISION TABLE, Y DIRECTION
*	 BIT 0 = CLIF1L
*	 BIT 1 = CLIF1R
*	 BIT 2 = CLIF2
*	 BIT 3 = CLIF3U
*	 BIT 4 = CLIF3L
*	 BIT 5 = CLIF3R
*	 BIT 6 = CLIF4
*	 BIT 7 = CLIF5
*
BCKYTB	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $00
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $10
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $20
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $30
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $40
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000011
	FCB	%00000011,%00000011,%00000011,%00000011
	FCB	%00000011,%00000011,%00000011,%00000011	CRT LINE $50
	FCB	%00000011,%00000011,%00000011,%00000111
	FCB	%00000111,%00000111,%00000111,%00000111
	FCB	%00000111,%00000111,%00000111,%00000100
	FCB	%00000100,%00000100,%00000100,%00000100	CRT LINE $60
	FCB	%00000100,%00000100,%00000100,%00000100
	FCB	%00000100,%00000100,%00000100,%00000100
	FCB	%00000100,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $70
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $80
	FCB	%00000000,%00000000,%00000000,%00001000
	FCB	%00001000,%00001000,%00001000,%00001000
	FCB	%00001000,%00001000,%00001000,%00001000
	FCB	%00111000,%00111000,%00111000,%00111000	CRT LINE $90
	FCB	%00111000,%00111000,%00111000,%00111000
	FCB	%00111000,%00111000,%00111000,%00111000
	FCB	%00111000,%00111000,%00111000,%00110000
	FCB	%00110000,%00110000,%00110000,%00110000	CRT LINE $A0
	FCB	%00010000,%00000000,%00000000,%00000000
	FCB	%00000000,%01000000,%01000000,%01000000
	FCB	%01000000,%01000000,%01000000,%01000000
	FCB	%01000000,%01000000,%01000000,%01000000	CRT LINE $B0
	FCB	%01000000,%01000000,%01000000,%01000000
	FCB	%01000000,%01000000,%01000000,%01000000
	FCB	%01000000,%01000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $C0
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $D0
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000	CRT LINE $E0
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000	CRT LINE $F0
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
*
*	TABLE FOR X DIRECTION, RANGE -$10 TO $128
*
BCKXS1	EQU	*
	FCB	%00010001,%00010001,%00010001,%00010001	CRT PIXEL -$20
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001	CRT PIXEL -$10
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001
*
* LABEL BCKXTB OR POSITION ZERO IN X DIRECTION
*
	FCB	%00010001,%00010001,%00010001,%00010001	CRT PIXEL $00
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001	CRT PIXEL $10
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001
	FCB	%00010001,%00010001,%00010001,%00010001	CRT PIXEL $20
	FCB	%00010000,%10010000,%10010000,%10010000
	FCB	%10010000,%10010000,%10010000,%10010000
	FCB	%10010000,%10010000,%10010000,%10010000
	FCB	%10010000,%10010000,%10010000,%10010000	CRT PIXEL $30
	FCB	%10010000,%10010000,%10010000,%10010000
	FCB	%10010000,%10010000,%10010000,%10010000
	FCB	%10010000,%10010000,%10010000,%10010000
	FCB	%10010000,%10010000,%10000000,%10000000	CRT PIXEL $40
	FCB	%10000000,%10000100,%10000100,%10000100
	FCB	%10000100,%10000100,%10000100,%10000100
	FCB	%10000100,%10000100,%10000100,%10000100
	FCB	%10000100,%10000100,%10000100,%10000100	CRT PIXEL $50
	FCB	%10000100,%10000100,%10000100,%10000100
	FCB	%10000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100	CRT PIXEL $60
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100	CRT PIXEL $70
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100	CRT PIXEL $80
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100	CRT PIXEL $90
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%11000100,%11000100	CRT PIXEL $A0
	FCB	%11000100,%11000100,%11000100,%11000100
	FCB	%11000100,%11000100,%10000100,%10000100
	FCB	%10000100,%10000100,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000	CRT PIXEL $B0
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000	CRT PIXEL $C0
	FCB	%10001000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000	CRT PIXEL $D0
	FCB	%10001000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000	CRT PIXEL $E0
	FCB	%10001000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001010
	FCB	%10001010,%10001010,%10101010,%10101010
	FCB	%00101010,%00101010,%00101010,%00101010	CRT PIXEL $F0
	FCB	%00101010,%00101010,%00101010,%00101010
	FCB	%00101010,%00101010,%00101010,%00101010
	FCB	%00101010,%00101010,%00101010,%00101010
	FCB	%00101010,%00101010,%00101010,%00101010	CRT PIXEL $100
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010	CRT PIXEL $110
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010	CRT PIXEL $120
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010	CRT PIXEL $130
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
BCKXS2	EQU	*
*
*	BACKGROUND BOX LANDING TABLE, Y DIRECTION
*	 BIT 0 = CLIF1L & CLIF1R
*	 BIT 1 = CLIF2
*	 BIT 2 = CLIF3R (TOP)
*	 BIT 3 = CLIF3L & CLIF3R
*	 BIT 4 = CLIF4
*	 BIT 5 = CLIF5
*	 BIT 6 = LAVA TROLLS LEFT & RIGHT
*
LNDYTB	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $00
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $10
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $20
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $30
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $40
	FCB	%00000000,%00000001,%00000001,%00000001
	FCB	%00000001,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000010,%00000010,%00000010	CRT LINE $50
	FCB	%00000010,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $60
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $70
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000100,%00000100,%00000100	CRT LINE $80
	FCB	%00000100,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00001000,%00001000
	FCB	%00001000,%00001000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $90
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00010000	CRT LINE $A0
	FCB	%00010000,%00010000,%00010000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $B0
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%00000000	CRT LINE $C0
	FCB	%00000000,%00000000,%00000000,%00000000
	FCB	%00000000,%00000000,%00000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10100000	CRT LINE $D0
	FCB	%10100000,%10100000,%10100000,%10100000
	FCB	%10100000,%10100000,%10100000,%10100000
	FCB	%10100000,%10100000,%10100000,%10100000
	FCB	%10100000,%10100000,%10100000,%10100000	CRT LINE $E0
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000	CRT LINE $F0
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	%10000000,%10000000,%10000000,%10000000
	FCB	$18	KZAPPER FUDGE
*
*	TABLE FOR X DIRECTION, RANGE -$10 TO $128
*
LNDXS1	EQU	*
	FCB	%10001001,%10001001,%10001001,%10001001	CRT PIXEL -$20
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001	CRT PIXEL -$10
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
*
*LABEL LNDXTB OR THE ZERO POINT ON SCREEN
*
LNDXS3	FCB	%10001001,%10001001,%10001001,%10001001	CRT PIXEL $00
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001	CRT PIXEL $10
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000
	FCB	%10001000,%10001000,%10001000,%10001000	CRT PIXEL $20
	FCB	%10001000,%10001000,%10001000,%00001000
	FCB	%00001000,%00001000,%00001000,%00001000
	FCB	%00001000,%00101000,%00101000,%00101000
	FCB	%00101000,%00101000,%00101000,%00101000	CRT PIXEL $30
	FCB	%00101000,%00101000,%00101000,%00101000
	FCB	%00101000,%00100000,%00100000,%00100000
	FCB	%00100000,%00100000,%00100000,%00100000
	FCB	%00100000,%00100000,%00100000,%00100000	CRT PIXEL $40
	FCB	%00100000,%00100000,%00100000,%00100000
	FCB	%00100000,%00100000,%00100000,%00100000
	FCB	%00100000,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010	CRT PIXEL $50
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00100010,%00100010,%00100010
	FCB	%00100010,%00110010,%00110010,%00110010	CRT PIXEL $60
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010	CRT PIXEL $70
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010	CRT PIXEL $80
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010	CRT PIXEL $90
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00110010,%00110010,%00110010
	FCB	%00110010,%00100010,%00100010,%00100010	CRT PIXEL $A0
	FCB	%00100010,%00100010,%00100000,%00100000
	FCB	%00100000,%00100000,%00100000,%00100000
	FCB	%00100000,%00100000,%00100000,%00100000
	FCB	%00100000,%00100000,%00100000,%00100000	CRT PIXEL $B0
	FCB	%00100000,%00100000,%00100000,%00100000
	FCB	%00100000,%00100000,%00100000,%00100000
	FCB	%00100000,%00100000,%00100000,%00100000
	FCB	%00100000,%00100100,%00100100,%00100100	CRT PIXEL $C0
	FCB	%00100100,%00100100,%00100100,%00100100
	FCB	%00100100,%00100100,%00100100,%00100100
	FCB	%00100100,%00100100,%00100100,%00100100
	FCB	%00100100,%00100100,%00100100,%00100100	CRT PIXEL $D0
	FCB	%00100100,%00100100,%00100100,%00100100
	FCB	%00100100,%00100100,%00100100,%00100100
	FCB	%00100100,%00100100,%00100100,%00100100
	FCB	%00100100,%00100100,%00100100,%00100100	CRT PIXEL $E0
	FCB	%00100100,%00100100,%00100100,%00000100
	FCB	%00000100,%00000100,%00000100,%00000100
	FCB	%00000100,%00000100,%00000100,%10000100
	FCB	%10000100,%10001100,%10001100,%10001100	CRT PIXEL $F0
	FCB	%10001101,%10001101,%10001101,%10001101
	FCB	%10001101,%10001101,%10001101,%10001101
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001	CRT PIXEL $100
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001	CRT PIXEL $110
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001	CRT PIXEL $120
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001	CRT PIXEL $130
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
	FCB	%10001001,%10001001,%10001001,%10001001
LNDXS2	EQU	*
*
* Bypass this copy protection (see the NOPs below)
*
KZAP:
	CLR	PRAM,U
	CLR	PRAM+1,U
	CLR	PRAM+2,U
L001_071:	PCNAP	1
	LDX	PRAM,U
	LDA	,X++
	ADDA	PRAM+2,U
	STA	PRAM+2,U
	STX	PRAM,U
	BPL	L001_071
	CMPA	KZAP1
	BEQ	KZAP

	IF CopyProtect
	JSR	RADDR
	ELSE
	NOP
	NOP
	NOP
	ENDIF

	BRA	KZAP
*
*******************************************************************************
*
*	KEN LANTZ CLIFF DESTROIER, COMPACT DATA TABLE
*	 (RUN LENGTH [UPPER NIBBLE], THEN COLOR NIBBLE [LOWER NIBBLE])
******************************************************************************
FIRSTI	FCB	$F0,$F0,$20,$01
	FCB	$80,$40+LIB,$40,$40+LIB,$E0,$01
	FCB	$F0+LIB,$F0+LIB,$20+LIB,$01
	FCB	$10,$F0+LIB,$60+LIB,$50+DKB,$40+LIB,$10,$01
	FCB	$20,$20+LIB,$50+DKB,$20+LIB,$50+MEB,$20+MEB,$20+LIB
	FCB	 $20+DKB,$60+MEB,$20,$01
	FCB	$30,$50+MEB,$30+DKB,$20+LIB,$30+MEB,$30+DKB,$20+MEB
	FCB	 $20+DKB,$40+MEB,$50,$01
	FCB	$40,$B0+MEB,$40+DKB,$90+MEB,$40,$01
	FCB	$80,$80+MEB,$30+DKB,$20+MEB,$B0,$01
	FCB	$D0,$20+MEB,$20+DKB,$40+MEB,$C0,$01
	FCB	$00
*SECOND IMAGE
	FCB	$10,$01
	FCB	$F0,$F0,$20,$01
	FCB	$F0,$F0,$20,$01
	FCB	$70,$50+LIB,$30,$50+LIB,$E0,$01
	FCB	$10,$A0+MEB,$B0+LIB,$50+DKB,$40+MEB,$10,$01
	FCB	$20,$20+MEB,$50+DKB,$90+MEB,$20+LIB,$20+DKB,$60+MEB,$20,$01
	FCB	$30,$30+MEB,$50+DKB,$50+MEB,$30+DKB,$20+MEB,$20+DKB
	FCB	 $40+MEB,$50,$01
	FCB	$40,$90+MEB,$80+DKB,$70+MEB,$40,$01
	FCB	$80,$60+MEB,$30,$20+DKB,$20+MEB,$B0,$01
	FCB	$D0,$10+MEB,$10+DKB,$20,$30+MEB,$C0,$01
	FCB	$00
*THIRD IMAGE
	FCB	$10,$01
	FCB	$10,$01
	FCB	$10,$01
	FCB	$F0,$F0,$20,$01
	FCB	$F0,$F0,$20,$01
	FCB	$70,$50+MEB,$30,$50+MEB,$E0,$01
	FCB	$10,$A0+DKB,$B0+MEB,$50+DKB,$20+MEB,$10+DKB,$10+MEB,$10,$01
	FCB	$20,$20+MEB,$50+DKB,$20+MEB,$30+DKB,$60+MEB,$20+DKB
	FCB	 $20+MEB,$40+MEB,$20,$01
	FCB	$30,$30+MEB,$70+DKB,$30+MEB,$30+DKB,$10+MEB,$10+MEB
	FCB	 $30+DKB,$30+MEB,$50,$01
	FCB	$40,$10+MEB,$20+MEB,$30+MEB,$20+MEB,$10+MEB,$80+DKB
	FCB	 $20+MEB,$30+DKB,$20+MEB,$40,$01
	FCB	$80,$10+MEB,$40+DKB,$10+MEB,$30,$20+DKB,$20+MEB,$B0,$01
	FCB	$F0,$F0,$20,$01
	FCB	$D0,$10+DKB,$10+DKB,$20,$30+DKB,$C0,$01
	FCB	$00
*FOURTH IMAGE
	FCB	$10,$01
	FCB	$10,$01
	FCB	$10,$01
	FCB	$10,$01
	FCB	$10,$01
	FCB	$F0,$F0,$20,$01
	FCB	$F0,$F0,$20,$01
	FCB	$70,$50+DKB,$30,$50+DKB,$E0,$01
	FCB	$10,$A0+DKB,$40+DKB,$30+MEB,$90+DKB,$10+MEB,$20+DKB
	FCB	 $10+DKB,$10+DKB,$10,$01
	FCB	$20,$20+MEB,$50+DKB,$20+MEB,$30+DKB,$20+MEB,$50+DKB
	FCB	 $30+MEB,$20+MEB,$40,$01
	FCB	$30,$30+MEB,$70+DKB,$30+MEB,$30+DKB,$10+MEB,$10+MEB
	FCB	 $30+DKB,$30+MEB,$50,$01
	FCB	$40,$10+DKB,$20+MEB,$30+DKB,$20+DKB,$10+MEB,$80+DKB
	FCB	 $20+DKB,$50+DKB,$40,$01
	FCB	$80,$10+DKB,$40+DKB,$10+DKB,$50,$20+DKB,$B0,$01
	FCB	$A0,$10+DKB,$20,$10+DKB,$10+DKB,$10,$10+DKB,$20
	FCB	 $10+DKB,$10+DKB,$C0,$01
	FCB	$B0,$10+DKB,$10,$10+DKB,$20,$20+DKB,$10,$C0,$01
	FCB	$00
*FIFTH IMAGE
	FCB	$10,$01
	FCB	$10,$01
	FCB	$10,$01
	FCB	$10,$01
	FCB	$10,$01
	FCB	$10,$01
	FCB	$10,$01
	FCB	$F0,$F0,$20,$01
	FCB	$F0,$F0,$20,$01
	FCB	$F0,$F0,$20,$01
	FCB	$10,$A0+DKB,$10,$20+DKB,$10,$30+DKB,$90,$10+DKB,$20
	FCB	 $10+DKB,$10+DKB,$10,$01
	FCB	$20,$20+DKB,$50+DKB,$20+DKB,$30+DKB,$20+DKB,$50,$30+DKB
	FCB	 $20+DKB,$40,$01
	FCB	$30,$30+DKB,$10,$10+DKB,$20,$10+DKB,$20,$10+DKB,$30+DKB
	FCB	 $30,$10+DKB,$10+DKB,$30,$30+DKB,$50,$01
	FCB	$40,$10+DKB,$20+DKB,$30+DKB,$20+DKB,$10+DKB,$20+DKB,$20
	FCB	 $10+DKB,$10,$10+DKB,$20,$10+DKB,$50,$20+DKB,$40,$01
	FCB	$80,$10+DKB,$40,$10+DKB,$50,$20+DKB,$B0,$01
	FCB	$F0,$F0,$20,$01
	FCB	$A0,$10+DKB,$20,$10+DKB,$10,$10,$10+DKB,$20,$10,$10+DKB,$C0,$01
	FCB	$C0,$10+DKB,$10,$10+DKB,$20,$10+DKB,$20,$C0,$01
	FCB	$00
KZAP1	FCB	$E8		KZAPPER DATA
;	LIST
*
AOFFL1	EQU	$45
AOFFL2	EQU	$81
AOFFL3	EQU	$D0
AOFFUD	EQU	$20
*
*	AUTO MATIC REMOVAL OF PLAYER
*
AUTOFF	LDD	PPOSX,U		OFF SCREEN?
	CMPD	#ELEFT+3
	BLT	AUTSCR
	CMPD	#ERIGHT-3
	BGT	AUTSCR
AUT2	LDA	#AOFFL1		ASSUME NEAR TRACKING LINE 1
	LDB	PPOSY+1,U
	CMPB	#AOFFL1+AOFFUD
	BLO	AUTTRK		 BR=TRACK ON LINE 1
	LDA	#AOFFL2		ASSUME NEAR TRACKING LINE 2
	CMPB	#AOFFL2+AOFFUD
	BLO	AUTTRK		 BR=TRACK ON LINE 2
	LDA	#AOFFL3		ASSUME NEAR TRACKING LINE 3
AUTTRK	CLRB			ASSUME ON TRACKING LINE
	SUBA	PPOSY+1,U	ABOVE OR BELOW LINE?
	BPL	AUTFLP		BR=AROUND THE LINE
	LDA	PVELY,U		ALREADY GOING UP?
	BMI	AUTFLP		 BR=YES, SO WAIT TILL NEXT TIME
	LDD	#AUTOFP		GET OFF GROUND OR JUST FLAP
	STD	PJOY,U
	LDB	#1
AUTFLP	LDA	PFACE,U		MOVE IN DIRECTION OF FACING
	BMI	AUTLF2
	LDA	#1
	STD	CURJOY
	RTS
*
AUTLF2	LDA	#-1
	STD	CURJOY
	RTS
*
AUTSCR	JSR	CPLYR		ERASE PLAYER FOR THE LAST TIME
	PCNAP	60		WAIT A SECOND
	LDX	#150		TRY TO START PLAYER AGAIN
	STX	PPOSX,U
	LDX	PDECSN,U
	JMP	[DCRE,X]
*
AUTOFP	LDD	#AUTOFF
	STD	PJOY,U
	CLRB
	BRA	AUTFLP
*
*
*	SOUND TABLE
*	 PIRORITY,SOUND,LENGTH (IF M.S.BIT SET ON SOUND, SOUND,LENGTH)
*		SOUND !N$00 DOES NOT DISTURBE THE SOUND, BUT EXTENDS TIMER				*  !N = NOT   !. = AND
*		SOUND !N$FF TURNS OFF (KILLS) THE SOUND														*  !N = NOT   !. = AND
*		SOUND !N$xx (RANGE 01-3E) SENDS THE CORRESPONDING SOUND						*  !N = NOT   !. = AND     !+ = OR
*
* If bit 7 of Sound is a 1 then it will play continue with the next two values in the list (Sound,Length) until bit 7 of sound is a 0 then it will stop
*
  IF Add_Audio
SNCRED	FCB	200,$75,40	  CREDIT CHANGE SOUND
SNGS	  FCB	200,$64,1	    GAME START
SNHI2	  FCB	190,$63,6	    HIGH SCORE TO DATE END SOUND
SNHIGH	FCB	190		  	    HIGH SCORE TO DATE
	FCB	    $EE,60    	$EE,$3C
	FCB	    $EF,20			$EF,$14
	FCB	    $EF,30			$EF,$1E
	FCB	    $F0,2*60		$F0,$78
	FCB	    $EF,60			$EF,$3C
	FCB	    $EF,100
	FCB	    $F0,4*60
	FCB	    $E5,20
	FCB	    $E5,100
	FCB	    $EF,60			,$3C
	FCB	    $F0,4*60
	FCB	    $E5,30
	FCB	    $E7,4*60
*
	FCB	    $EF,80
	FCB	    $EF,60			,$3C
	FCB	    $EF,20
	FCB	    $F0,4*60
	FCB	    $EF,20
	FCB	    $EF,60
	FCB	    $EF,80
	FCB	    $EF,60
	FCB	    $EF,20
	FCB	    $EF,60
	FCB	    $F0,4*60
	FCB	    $EF,20
	FCB	    $EF,60
	FCB	    $EF,80
	FCB	    $EF,60
	FCB	    $EF,20
	FCB	    $EF,60
	FCB	    $EF,80
	FCB	    $F0,2*60
	FCB	    $70,4*60
SNREPL	FCB	100,$74,90	EXTRA MAN
  IF DelayCliffDestroyerSound
SNCLIF  FCB 67,128+00,78  Delay for a (60*1.3)=78 = 1.3 seconds
	      FCB	      $66,90	CLIFF DESTROYER
  ELSE
SNCLIF	FCB	067,$66,90	CLIFF DESTROYER
  ENDIF
SNPTED	FCB	066,$E9,15,$E9,15	PTERODACTYL DYING SOUND
	      FCB	    $E9,07,$E9,7
      	FCB	    $69,90
SNPTEI	FCB	065,$DB,30,$5A,30	PTERODACTYL INTRODUCTION SCREAM
SNPTE	  FCB	065,$5B,120		PTERODACTYL SCREAM
SNBOUN	FCB	050,$63,60	COLLECT BOUNTY
SNTROL	FCB	050,$76,30	CAPTURED BY LAVA TROLL SOUND
SNEGG	  FCB	045,$7C,30	PLAYER HITS EGG SOUND
SNEGGH	FCB	045,$7D,30	EGG HATCHING SOUND
SNMOUN	FCB	045,$73,30	ENEMY MOUNTING THE BIRD
SNCTHD	FCB	010,$79,10	CLIFF THUD
*
SNECRE	FCB	040,$F8,90,$00,1	ENEMY RE-CREATED (TRANSPORTER)
SNEDIE	FCB	040,$69,20	ENEMY DIES
SNELAV	FCB	040,$72,30	ENEMY IN LAVA
SNETHD	FCB	009,$F7,30,$00,1	ENEMIES THUD
SNELWU	FCB	006,$5E,60	ENEMIES WING UP SOUND
SNELWD	FCB	006,$5F,60	ENEMIES WING DOWN SOUND
SNERU1	FCB	006,$5D,60	ENEMY RUNS
SNERU2	FCB	006,$5C,60	ENEMY RUNS
SNEMSK	FCB	006,$59,60	ENEMIES SKIDDING SOUND
SNEMS2	FCB	006,$58,30	END OF ENEMIES SKIDDING SOUND
SNEFAL	FCB	006,$00,1	ENEMY STARTING TO FALL (SKID STOP)
*
SNPDIE	FCB	080,$69,20	PLAYER DIES
SNPCR1	FCB	070,$ED,30	PLAYER 1 RE-CREATED (TRANSPORTER)
;      	FCB	    $EB,255	PLAYER FADING IN  (TRANSPORTER)  Original was 255 = 4.25 seconds
; $EB is .744 seconds = 44 jiffies
; $EA is same exact length as $EB
* Alternate between $EB & $EA for 255 = 4.25 seconds
        FCB $EB,44
        FCB $EB,44
        FCB $EB,42
        FCB $EB,40
        FCB $EB,40
        FCB $EB,38
        FCB $EB,38
        FCB $EB,27

        FCB $EA,25
        FCB $EA,23
        FCB $EA,18
        FCB $EA-128,10 * Bit 7 low = end of sounds to play
;      	FCB	    $7F,165 Original was 165 = 2.75 seconds
* Speed it up here
SNPCR2	FCB	070,$ED,30+13	PLAYER 2 RE-CREATED (TRANSPORTER)
;      	FCB	    $EA,255	PLAYER FADING IN  (TRANSPORTER)
;      	FCB	    $7F,165-13
* Alternate between $EA & $EB for 255 = 4.25 seconds
        FCB $EA,44
        FCB $EA,44
        FCB $EA,42
        FCB $EA,40
        FCB $EA,40
        FCB $EA,38
        FCB $EA,38
        FCB $EA,27

        FCB $EB,25
        FCB $EB,23
        FCB $EB,18-4
        FCB $EB-128,1 * Bit 7 low = end of sounds to play
;      	FCB	    $7F,165 Original was 165 = 2.75 seconds
SNPTREF	FCB	070,$00,1	PLAYER ABORTED FADING IN  (TRANSPORTER)
SNPLAV	FCB	060,$72,30	PLAYER IN LAVA
SNPTHD	FCB	020,$F7,30,$00,1	AT LEAST 1 PERSON THUD'ED
SNPLWU	FCB	010,$5E,90	PLAYERS WING UP SOUND   010=$0A,   $5E=$5E   ,$5A   *  !N = NOT   !. = AND
SNPLWD	FCB	010,$5F,90	PLAYERS WING DOWN SOUND 010=$0A,   $5F=$5F   ,$5A
SNPRU1	FCB	010,$5D,70	PLAYER RUNS							010=$0A,   $5D=$5D   ,$46
SNPRU2	FCB	010,$5C,70	PLAYER RUNS							010=$0A,   $5C=$5C   ,$46
SNPLSK	FCB	010,$59,70	PLAYERS SKIDDING SOUND	010=$0A,   $59=$59   ,$46
SNPLS2	FCB	010,$58,40	END OF PLAYERS SKIDDING SOUND  			010=$0A,   $58=$58   ,$28
SNPFAL	FCB	010,$00,60	PLAYER STARTING TO FALL (SKID STOP)	010=$0A,   $00=$00   ,$3C

  ELSE
SNCRED	FCB	200,$75,40	CREDIT CHANGE SOUND
SNGS	  FCB	200,$64,1	GAME START
SNHI2	  FCB	190,$63,6	HIGH SCORE TO DATE END SOUND
SNHIGH	FCB	190			HIGH SCORE TO DATE
	FCB	    $EE,60    	$EE,$3C
	FCB	    $EF,20			$EF,$14
	FCB	    $EF,30			$EF,$1E
	FCB	    $F0,2*60		$F0,$78
	FCB	    $EF,60			$EF,$3C
	FCB	    $EF,100
	FCB	    $F0,4*60
	FCB	    $E5,20
	FCB	    $E5,100
	FCB	    $EF,60			,$3C
	FCB	    $F0,4*60
	FCB	    $E5,30
	FCB	    $E7,4*60
*
	FCB	    $EF,80
	FCB	    $EF,60			,$3C
	FCB	    $EF,20
	FCB	    $F0,4*60
	FCB	    $EF,20
	FCB	    $EF,60
	FCB	    $EF,80
	FCB	    $EF,60
	FCB	    $EF,20
	FCB	    $EF,60
	FCB	    $F0,4*60
	FCB	    $EF,20
	FCB	    $EF,60
	FCB	    $EF,80
	FCB	    $EF,60
	FCB	    $EF,20
	FCB	    $EF,60
	FCB	    $EF,80
	FCB	    $F0,2*60
	FCB	    $70,4*60
SNREPL	FCB	100,$74,90	EXTRA MAN
  IF DelayCliffDestroyerSound
SNCLIF  FCB 67,128+00,78  Delay for a (60*1.3)=78 = 1.3 seconds
	      FCB	      $66,90	CLIFF DESTROYER
  ELSE
SNCLIF	FCB	067,$66,90	CLIFF DESTROYER
  ENDIF
SNPTED	FCB	066,$E9,15,$E9,15	PTERODACTYL DYING SOUND
	      FCB	    $E9,07,$E9,7
      	FCB	    $69,90
SNPTEI	FCB	065,$DB,30,$5A,30	PTERODACTYL INTRODUCTION SCREAM
SNPTE	  FCB	065,$5B,120		PTERODACTYL SCREAM
SNBOUN	FCB	050,$63,60	COLLECT BOUNTY
SNTROL	FCB	050,$76,30	CAPTURED BY LAVA TROLL SOUND
SNEGG	  FCB	045,$7C,30	PLAYER HITS EGG SOUND
SNEGGH	FCB	045,$7D,30	EGG HATCHING SOUND
SNMOUN	FCB	045,$73,30	ENEMY MOUNTING THE BIRD
SNCTHD	FCB	010,$79,10	CLIFF THUD
*
SNECRE	FCB	040,$F8,90,$00,1	ENEMY RE-CREATED (TRANSPORTER)
SNEDIE	FCB	040,$69,20	ENEMY DIES
SNELAV	FCB	040,$72,30	ENEMY IN LAVA
SNETHD	FCB	009,$F7,30,$00,1	ENEMIES THUD
SNELWU	FCB	006,$5E,60	ENEMIES WING UP SOUND
SNELWD	FCB	006,$5F,60	ENEMIES WING DOWN SOUND
SNERU1	FCB	006,$5D,60	ENEMY RUNS
SNERU2	FCB	006,$5C,60	ENEMY RUNS
SNEMSK	FCB	006,$59,60	ENEMIES SKIDDING SOUND
SNEMS2	FCB	006,$58,30	END OF ENEMIES SKIDDING SOUND
SNEFAL	FCB	006,$00,1	ENEMY STARTING TO FALL (SKID STOP)
*
SNPDIE	FCB	080,$69,20	PLAYER DIES
SNPCR1	FCB	070,$ED,30	PLAYER 1 RE-CREATED (TRANSPORTER)
      	FCB	    $EB,255	PLAYER FADING IN  (TRANSPORTER)
      	FCB	    $7F,165
SNPCR2	FCB	070,$ED,30+13	PLAYER 2 RE-CREATED (TRANSPORTER)
      	FCB	    $EA,255	PLAYER FADING IN  (TRANSPORTER)
      	FCB	    $7F,165-13
SNPTREF	FCB	070,$00,1	PLAYER ABORTED FADING IN  (TRANSPORTER)
SNPLAV	FCB	060,$72,30	PLAYER IN LAVA
SNPTHD	FCB	020,$F7,30,$00,1	AT LEAST 1 PERSON THUD'ED
SNPLWU	FCB	010,$5E,90	PLAYERS WING UP SOUND   010=$0A,   $5E=$5E   ,$5A   *  !N = NOT   !. = AND
SNPLWD	FCB	010,$5F,90	PLAYERS WING DOWN SOUND 010=$0A,   $5F=$5F   ,$5A
SNPRU1	FCB	010,$5D,70	PLAYER RUNS							010=$0A,   $5D=$5D   ,$46
SNPRU2	FCB	010,$5C,70	PLAYER RUNS							010=$0A,   $5C=$5C   ,$46
SNPLSK	FCB	010,$59,70	PLAYERS SKIDDING SOUND	010=$0A,   $59=$59   ,$46
SNPLS2	FCB	010,$58,40	END OF PLAYERS SKIDDING SOUND  			010=$0A,   $58=$58   ,$28
SNPFAL	FCB	010,$00,60	PLAYER STARTING TO FALL (SKID STOP)	010=$0A,   $00=$00   ,$3C
  ENDIF

*
ENDA1A	EQU	ENDAD1	CURRENT END ADDRESS OF $6000 - $8FFF
ENDA2A	EQU	ENDAD2	CURRENT END ADDRESS OF $D800 - $DFFF
ENDAD3	EQU	*	CURRENT END ADDRESS OF $E6A8 - $EFF0
;	IFGE	*-$EFF0
;	 FCB	$1111	ERROR, $E6A8-$EFF0 MODULE IS TOO LONG
;	ENDIF
;	END	VINIT2

*
;	ORG	$EFF0
;	FDB	SYSV,SYSV,SYSV,SYSV,IRQ,SYSV,SYSV,SYSV
;	END	SYSV

**** T12REV3.SRC.out ****

;	NAM	F000 TEST ROM FOR 'JOUST' INCLUDING AUDITING AND ADJUSTMENTS

;	INCLUDE RAMDEF.SRC
;	INCLUDE SHRAMDEF.SRC
;	INCLUDE EQU.SRC
;	INCLUDE MESSEQU.SRC
;	INCLUDE MESSEQU2.SRC

	SETDP	$A0

;	ORG	TSTORG
PWRUPV:
	JMP	PWRUP
	JMP	ADVSW_Dollar

	IF AdvanceSwitch
	JMP	AUTCYC
	ELSE
	JMP	PWRUP
	ENDIF

	JMP JNAP
	JMP	RAND_T12
	JMP	TEST
	JMP	CYCLE

*COLOR RAM TABLE
*
* Joust has 2 bits for Blue just like the CoCo 3 but has 3 bits for both Red and Green, so there is more fidelity with Jousts Colours
* Joust Colour bits  = BbGggRrr
* CoCo 3 Colour bits = xxRGBrgb
* This pallete is used when Joust starts for RAM test screen
CRTAB
	FCB	$00  ;$00	0 SPACED OUT
	FCB	$1B  ;$F9	1 LASER (SPECIAL)
	FCB	$24  ;$07	2 RED
	FCB	$10  ;$28	3 GREEN
	FCB	$34  ;$2F	4 YELLOW
	FCB	$00  ;$00	5 MESSAGES
	FCB	$38  ;$A4	6 GRAY
	FCB	$22  ;$15	7 TERRAIN
	FCB	$2D  ;$C7	8 PURPLE
	FCB	$3F  ;$FF	9 WHITE
	FCB	$12  ;$38	A BOMB CYCLER
	FCB	$26  ;$17	B ORANGE
	FCB	$29  ;$CC	C CYCLER
	FCB	$08  ;$81	D TIE1
	FCB	$08  ;$81	E TIE2
	FCB	$34  ;$2F	F TIE3,MONO
;CRTAB
;	FCB	0	0 SPACED OUT
;	FCB	$F9	1 LASER (SPECIAL)
;	FCB	$07	2 RED
;	FCB	$28	3 GREEN
;	FCB	$2F	4 YELLOW
;	FCB	$00	5 MESSAGES
;	FCB	$A4	6 GRAY
;	FCB	$15	7 TERRAIN
;	FCB	$C7	8 PURPLE
;	FCB	$FF	9 WHITE
;	FCB	$38	A BOMB CYCLER
;	FCB	$17	B ORANGE
;	FCB	$CC	C CYCLER
;	FCB	$81	D TIE1
;	FCB	$81	E TIE2
;	FCB	$2F	F TIE3,MONO


*
***	POWER UP ROUTINE
*
PWRUP
;	ORCC	#$FF	NO INTERRUPTS ETC.
;	LDS	#HSTK	IF AND WHEN WE DECIDE TO USE THE STACK
;	CLR	PIA0+1
;	CLR	PIA0
;	LDA	#$3C	SET LED 3 TO 1 (WE'RE MAKING AN F TO BLANK)
;	STA	PIA0+1
;	CLR	PIA1+1
;	LDA	#$C0	BITS 6,7 TO OUTPUTS (LEDS)
;	STA	PIA1
;	LDA	#$3C	SET BIT 2 TO 1
;	STA	PIA1+1	CLEAR CB2 (LED)
;	LDA	#$C0	CLEAR LEDS
;	STA	PIA1	SET BITS 0,1
;	LDA	#1
;	STA	RWCNTL	MAKE SURE WE COPY FROM ROM!
;	LDX	#CRTAB
;	LDY	#CRAM
;PWRUP0	LDD	,X++
;	STD	,Y++
;	CMPX	#CRTAB+16
;	BLO	PWRUP0
;	LDA	#2
;	LDY	#PWRUP1
;	LDX	#0
;	BRA	RAMTST	DO A RAM TEST.
;PWRUP1	LDY	#PWRUP4
;	JMP	ROMTST
;PWRUP4	LDA	#$34	PUT A ZERO IN THE LED.
;	STA	PIA0+1
;	STA	PIA1+1
;	CLR	PIA1
	LDA	#RAM/256    ;	LDA	#RAM!>8
	TFR	A,DP
	LDS	#HSTK
	JSR	SCCLR          * CLEAR SCREEN
	LDA	#MSIND	INITIAL TESTS INDICATE
	LDX	#$3070
	LDB	#$99
	JSR	OUTPHR
	LDA	#MSALL	ALL SYSTEMS GO
	LDX	#$3A90
	LDB	#$99
	JSR	OUTPHR	INDICATE OK
  LDY	#InitGame   *	was LDY	#CKCMSV	DELAY TO JUMP INTO GAME.
	LDA	#7
	JMP	DELA1

*
*	RAM TEST....Y = PLACE TO RETURN IF NO ERROR
*	X = SEED TO START WITH
*	A = ITERATIONS TO MAKE

*
* Removed RAMTST code to make space for my code
*
RAMTST:
	IF RAMTEST
	INCLUDE	"./RAMTEST.SRC"
	ELSE
	JMP	,Y	ALL ROMS OK...RETURN.
	ENDIF

DELAY	LDA	#32
DELA1	LDX	#$5800
DELA2	LEAX	-1,X
	LDB	#WDATA
	STB	WDOG
	CMPX	#0
	BNE	DELA2
	DECA
	BNE	DELA1
	JMP	,Y	RETURN

*
*	PULSE......PUT SOME CRAP IN LED'S
*	LOW HALF OF A IS ERROR CODE...
*	BOTH HALVES OF B ARE THE RELEVANT DATA.
*

PULSE	TFR	D,U	SAVE A,B
	LDA	#2
	TFR	A,DP
PULSE0	TFR	U,D
	LDY	#PULSE1
	JMP	PSSUB
PULSE1	LDA	#2
	LDY	#PULSE2
	BRA	DELA1
PULSE2	LDY	#PULSE3
	BRA	BLKLED
PULSE3	LDA	#1
	LDY	#PULSE4
	BRA	DELA1
PULSE4	TFR	U,D	GET DATA BACK
	TFR	B,A	SHIFT B DOWN
	LSRA
	LSRA
	LSRA
	LSRA		PROPER HALF
	LDY	#PULSE5
	BRA	PSSUB	PULSE IT.
PULSE5	LDA	#2
	LDY	#PULSE6
	BRA	DELA1
PULSE6	LDY	#PULSE7
	BRA	BLKLED
PULSE7	LDA	#1
	LDY	#PULSE8
	BRA	DELA1
PULSE8	TFR	U,D
	TFR	B,A
	LDY	#PULSE9
	BRA	PSSUB
PULSE9	LDA	#2
	LDY	#PULS10
	BRA	DELA1
PULS10	LDY	#PULS11
	BRA	BLKLED
PULS11	LDA	#5
	LDY	#PULS12
	BRA	DELA1
PULS12	TFR	DP,A	SEE IF FIRST PASS??
	DECA
	TFR	A,DP
	BNE	PULSE0
	LDY	#PULS95
	BRA	BLKLED	BLANK EM.
PULS95	TFR	U,D	RESTORE D
	JMP	,S	AND RETURN

BLKLED	LDA	#$3C
	STA	PIA0+1
	INCA
	STA	PIA1+1
	LDA	#$C0
	STA	PIA1
	JMP	,Y	AND RETURN

*	PSSUB - PUT LOW HALF OF A TO LEDS

PSSUB	TFR	A,B	SAVE A COPY.
	RORA		BIT 0 - CARRY
	RORA		BIT 0 - BIT 7
	RORA		BIT 0 - BIT 6
	ANDA	#$C0
	STA	PIA1	THOSE BITS OUT.
	LDA	#$34	ASSUME ZERO
	BITB	#$4	SEE IF 1
	BEQ	PSSUB1	NOPE
	LDA	#$3C
PSSUB1	STA	PIA1+1	THATS ALL FOR THAT BIT
	LDA	#$34
	BITB	#$8
	BEQ	PSSUB2
	LDA	#$3C
PSSUB2	STA	PIA0+1
	JMP	,Y	AND RETURN

*
* Removed ROMTST code to make space for my code
*
ROMTST:
	IF ROMTEST
	INCLUDE	"./ROMTEST.SRC"
	ELSE
	JMP	,Y	ALL ROMS OK...RETURN.
	ENDIF
*
*	ADVANCE SWITCH
*

*
* Removed ADVSW code to make space for my code
*
ADVSW_Dollar:
	IF AdvanceSwitch
	INCLUDE	"./Advance_Menu.SRC"
	ELSE

AVWAIT	LDA	PIA0	SEE IF ADVANCE PRESSED
	BITA	#$2	WELL??
	BNE	AVWAT2
	LDA	#$01
	BSR	JNAP
	BRA	AVWAIT	WELL, WAIT FOR IT.
AVWAT2	CLR	CRAM
	JSR	SCCLR          * CLEAR SCREEN
	BRA	AVWAT5
AVWAT4	LDA	PIA0
	BITA	#$2	WAIT FOR IT TO BE RELEASED
	BEQ	AVWAT3
AVWAT5	LDA	#$01
	BSR	JNAP
	BRA	AVWAT4
AVWAT3	RTS

AVCHK	LDA	PIA0
	BITA	#$2
	BEQ	AVCHK1
	ORCC	#$01	SEC
	RTS
AVCHK1	ANDCC	#$FE	CLC
	RTS

JNAP	LDB	#WDATA
	LDX	#$0300
LT12001_001:	STB	WDOG
	LEAX	-1,X
	BNE	LT12001_001
	DECA
	BPL	JNAP
	RTS

	ENDIF

*
*
*	DIVIDE PLAY TIME BY CREDITS
*	 INPUT: BCDN RMB 3 - BCD PLAYTIME (M.S.DIGIT BCDN+3)
*	 	BCDD RMB 3 - BCD CREDITS (M.S.DIGIT BCDD+3)
*	OUTPUT: REG.A = BCD MINUTES
*		BCDN+0 = BCD SECONDS
*	DESTROIES REG.B
*
DIVPTC	PSHS	X,Y
	BSR	BCDDIV
	LDD	BCDN+0	OVERFLOWING ANSWER?
	BEQ	LT12003_001	 BR=NO
	LDA	#$99	YES, INDICATE OVERFLOW
	PSHS	A
	STA	BCDN+2
	CLR	BCDN+1
	CLR	BCDN+0
	BRA	PTCRTS
*
LT12003_001:	LDA	BCDN+2	SAVE MINUTES
	PSHS	A
	LDD	BCDR+0
	STD	BCDN+0
	LDA	BCDR+2
	STA	BCDN+2
	LDD	#0
	STD	BCDR+0
	STA	BCDR+2
	LDA	#4	CALCULATE SECONDS FROM REMAINDER
LT12001_003:	ASL	BCDN+2	MULTIPLY BY 10
	ROL	BCDN+1
	ROL	BCDN+0
	ROL	BCDR+2	ALSO, USE CARRY
	DECA
	BNE	LT12001_003
	LDX	#BCDN+3	PREPARE TO MULTIPLY BY 6 (FINAL MULTIPLY BY 60)
	LDY	#BCDN+3
	LDU	#BCDN+3
	BSR	BCDAD2		TIMES 2
	LDD	BCDR+2
	STD	BCDCNT+0
	LDD	BCDN+1
	STD	BCDTMP+1
	LDX	#BCDCNT+4
	BSR	BCDAD2		TIMES 4
	BSR	BCDAD2		TIMES 6
	BSR	BCDDI2		DIVIDE TO GET SECONDS
PTCRTS	PULS	A,X,Y,PC
*
*	BCD ADDITION IN 8 BCD DIGITS
*	DESTROIES REG.D
*	PERFORMS THUS ([REG.U] = [REG.X] + [REG.Y])
*	REG.U, REG.Y & REG.X POINTS TO L.S.BYTE+1
*
BCDAD2	PSHS	X,Y,U
	LDB	#4	8 BCD DIGITS TO ADD
	BRA	BCDADC
*
*	BCD ADDITION IN 6 BCD DIGITS
*	DESTROIES REG.D
*	PERFORMS THUS ([REG.U] = [REG.X] + [REG.Y])
*	REG.U, REG.Y & REG.X POINTS TO L.S.BYTE+1
*
BCDADD	PSHS	X,Y,U
	LDB	#3	6 BCD DIGITS TO ADD
BCDADC	ANDCC	#$FE	CLEAR CARRY
LT12001_004:	LDA	,-X
	ADCA	,-Y
	DAA
	STA	,-U
	DECB
	BNE	LT12001_004
	PULS	X,Y,U,PC
*
*	BCD DIVIDE ROUTINE (BCDDIV & BCDDI2)
*	 INPUT: BCDN RMB 3 = 6 BCD DIGIT NUMERATOR (UNSIGNED)
*	 	BCDD RMB 3 = 6 BCD DIGIT DENOMINATOR (UNSIGNED)
*	 OUTPUT: BCDN   RMB 3 = THE ANSWER IN BCD
*		 BCDN+3 RMB 3 = THE REMAINDER IN BCD
*	DESTROIES: REG.D, REG.X, REG.Y
*
BCDDIV	LDD	#0
	STD	BCDR+0	CLEAR REMAINDER OF DIVIDE
	STA	BCDR+2
BCDDI2	LDD	BCDD	DIVIDE BY ZERO?
	BNE	BDVDIV	 BR=NO
	LDA	BCDD+2
	BNE	BDVDIV
	LDD	#0	YES, GIVE A RESULT OF ZERO
	STD	BCDN+0
	STA	BCDN+2
	RTS
*
BDVDIV	LDA	#6+1	THE WORLD WAS CREATED IN 6 DIGITS!
	STA	BCDCNT
	LDX	#BCDR+3	PRE-LOAD REGISTERS FOR SUBTRACTION ROUTINE
	LDY	#BCDD+3
	LDU	#BCDTMP+3
	BSR	BCDNEG	MAKE DENOMINATOR NEGATIVE VIA 10'S COMPLIMENT
BDVLP	DEC	BCDCNT	ANY MORE DIGITS LEFT?
	BNE	LT12002_001	 BR=YES
	BRA	BCDNEG	RESTORE DENOMINATOR
*
LT12002_001:	LDA	#4	BCD NIBBLE SHIFT
LT12001_005:	ASL	BCDN+2
	ROL	BCDN+1
	ROL	BCDN+0
	ROL	BCDR+2
	ROL	BCDR+1
	ROL	BCDR+0
	DECA
	BNE	LT12001_005
BDVCMP	BSR	BCDADD	SUBTRACT OFF THE LARGE DENOMINATOR
	BCS	BDVOK	 BR=SUBTRACT WAS GOOD ENOUGH
	BRA	BDVLP
*
BDVOK	LDD	BCDTMP+0	NEW REMAINDER
	STD	BCDR+0
	LDA	BCDTMP+2
	STA	BCDR+2
	INC	BCDN+2	NBR OF SUCCESSFULL SUBTRACTIONS GOES TO THE ANSWER
	BRA	BDVCMP	CHACK AGAIN FOR SUBTRACTION (MAXIMUM LOOPS = 9)
*
*	TO BCD SUBTRACT, NEGATE THEN ADD IN 6 BCD DIGITS
*	DESTROIES REG.D
*	PERFORMS THUS ([REG.Y] = NOT([REG.Y]) + 1)
*	REG.Y POINTS TO L.S.BYTE+1
*
BCDNEG	PSHS	Y
	LDB	#3	COMPLIMENT THE BCD NUMBER
LT12002_002:	LDA	#$99	DIGIT BY DIGIT (THERE MUST BE A FASTER WAY!)
	SUBA	,-Y
	STA	,Y
	DECB
	BNE	LT12002_002
	LDY	,S	RESTORE REG.Y
	LDB	#3	6 BCD DIGITS TO SUBTRACT
	ORCC	#$01	SET CARRY TO ADD 1
LT12001_006:	LDA	-1,Y
	ADCA	#0
	DAA
	STA	,-Y
	DECB
	BNE	LT12001_006
	PULS	Y,PC
*


ADJUST	LDX	#SPECFN	ZERO OUT THE SPECIAL FUNCTION BYTES
ADJS11	CLR	,X+
	CMPX	#ENDADJ
	BLO	ADJS11
	JSR	SCCLR	CLEAR THE SCREEN
	JSR	AVCHK
	BCC	LT12003_002
	JSR	AVWAIT	WAIT FOR HIM TO LET GO.
LT12003_002:	LDA	#TXADJT
	JSR	TEXT	PRINT THE ADJUSTMENT MESSAGE
	LDU	#CMOS
	LDX	#$1A20
	LDA	#MSADJS	FIRST OF THE AUDIT MESSAGES
LT12004_000:	PSHS	A,X	SAVE MESSAGE NUMBER
	LDB	#$22
	JSR	OUTP35	PRINT IT.
	STB	COLR
	STB	BCDR
	TFR	X,Y
	JSR	OUTCMOS
	LEAU	2,U
	LDB	#WDATA
	STB	WDOG
	PULS	A,X	RESTORE A
	LEAX	$A,X
	INCA
	CMPU	#ENDADJ
	BLT	LT12004_000
	LDA	#TXBADJ	PUT OUT THE MESSAGES AT THE BOTTOM OF THE SCREEN
	JSR	TEXT35	IN 3X5 CHARACTER FORMAT
	CLR	BCDR

	LDA	COINSL+1
	ANDA	#$0F
	CMPA	#9
	BNE	CURSOR
	LDA	#MSFRPLY
	LDB	COLR
	LDX	#$703E
	JSR	OUTP35

CURSOR	LDY	#$1620	FIRST CURSOR POSITION
	LDU	#CMOS	GET THE FIRST CMOS ADJUSTABLE LOCATION
	TFR	Y,X	GET THE CURSOR POSITION
	LDA	#CCURSR	GET THE CURSOR CHARACTER
	LDB	#$33	GET THE COLOR
	STD	SW0ST	SAVE THE CURSOR COLOR AND CHARACTER OFFSET IN MEMORY
	JSR	OUTC35	WRITE THE 3X5 CURSOR OUT
LT12001_007:	LDA	#WDATA	LET'S GO AND PET THE FUCKING DOG..........
	STA	WDOG
	LDA	PIA2	CHECK THE SWITCHES
	ANDA	#$03	WE ONLY WANT BITS 0&1 (THIS IS FOR JOUST)
	BEQ	LT12003_003	BRA= NO
	BSR	CSWIT	SEE WHO IT IS
LT12003_003:	LDB	#$34	FLIP TO THE COCKTAIL SIDE
	STB	PIA3+1
	LDA	PIA2	GET THE REST OF THEM
	ANDA	#$03	WE ONLY WANT BITS 0&1 (THIS IS FOR JOUST)
	BEQ	LT12002_003	BRA=NO
	JSR	CSWIT1	SEE WHO THEY ARE
LT12002_003:	LDB	#$3C	SET BACK TO THE NORMAL SIDE
	STB	PIA3+1
	JSR	AVCHK	CHECK TO SEE IF THE ADVANCE SWITCH HAS BEEN PRESSED
	BCC	LT12001_007	NOT EVEN AN ADVANCE SO LETS CHECK THEM AGAIN
	JSR	AVWAIT
	JMP	ADVSCV	GO START EXEC UP AND THE GAME


CSWIT	STA	SW3ST	SAVE THE SWITCH
	CMPA	SW2ST	GET THE LAST SWITCH HE HIT
	BEQ	LT12001_008	BRA= YES
	CLR	SW2SCN	CLEAR THE B PORT
	LDA	SW3ST	GET THE SWITCH
	STA	SW2ST	SAVE THE SWITCH
	LDA	#$2		NAP ONLY 16 MS
	JSR	JNAP	AND TAKE A CAT NAP
	LDA	PIA2	READ THE PORT TO MAKE SURE THAT IT IS VALID
	BNE	LT12003_004
LT12001_008:	LDB	#$05	TIME TO WAIT
	STB	SW3SCN	SAVE IT
LT12005_000:	LDA	PIA2	READ THE PORT
	BNE	LT12002_004	BRA= STILL PRESSED
	CLR	SW2ST	SAY THAT NO SWITCH HAS BEEN PRESSED
	RTS		TOO BAD YOU LET GO
LT12002_004:	LDA	#$2		NAP ONLY 16 MS
	JSR	JNAP	AND TAKE A CAT NAP
	DEC	SW3SCN	ONE LESS TIME
	BNE	LT12005_000	NOT DONE SO CHECK AGAIN
	LDA	PIA2	READ ONCE AGAIN

LT12003_004:	BITA	#$02	DID HE PRESS MOVE LEFT
	BNE	LT12004_001	BRA= YES HE DID SO MOVE THE CURSOR UP
	BITA	#$01
	BEQ	NOMOVE
	CMPY	#AEND	IS HE AT THE BOTTOM
	BEQ	NOMOVE	BRA= YES SO DON'T DO A THING
	BSR	NOCURS	ERASE THE CURSOR
	LEAY	-$A,Y	NOT UP SO IT MUST BE DOWN
	LEAU	-2,U	MOVE TO THE NEXT CMOS LOCATION
	CMPU	#MINUNT	IS HE IN THE MIDDLE OF PRICE SELECTION
	BNE	LT12010_000
	TST	SEED	HE'S NOT IN CUSTOM ADJUST
	BEQ	LT12010_000
	LEAY	-$3C,Y	BIG JUMP
	LEAU	-$C,U	IN CMOS TOO..
LT12010_000:	BRA	CURS	WRITE THE CURSOR OUT IN IT'S NEW POSITION
LT12004_001:	CMPY	#ATOP	IS HE AT THE TOP OF THE ADJUSTMENTS
	BEQ	NOMOVE	BRA= YES SO DON'T DO A THING
	BSR	NOCURS	ERASE THE CURSOR
	LEAY	$A,Y	MOVE THE CURSOR UP
	LEAU	2,U	MOVE BACK A CMOS LOCATION
	CMPU	#SLOT1M	IS HE IN THE MIDDLE OF PRICE SELECTION
	BNE	CURS
	TST	SEED	HE'S NOT IN CUSTOM ADJUST
	BEQ	CURS
	LEAY	$3C,Y	BIG JUMP
	LEAU	$C,U	IN CMOS TOO..

CURS	TFR	Y,X	GET THE CURSOR
	LDD	SW0ST	GET THE DATA BACK
	JSR	OUTC35	WRITE THE 3X5 CURSOR OUT
NOMOVE	CLRA
	JSR	JNAP
	RTS

NOCURS	TFR	Y,X	GET THE CURSOR
	LDA	SW0ST	GET THE DATA BACK
	CLRB
	JSR	OUTC35	WRITE THE 3X5 CURSOR OUT
	RTS


CSWIT1	STA	SW3ST	SAVE THE SWITCH
	CMPA	SW2STI	GET THE LAST SWITCH HE HIT
	BEQ	LT12001_009	BRA= YES
	CLR	SW2SCI	CLEAR THE B PORT
	LDA	SW3ST	GET THE SWITCH
	STA	SW2STI	SAVE THE SWITCH
	LDA	#$2		NAP ONLY 16 MS
	JSR	JNAP	AND TAKE A CAT NAP
	LDA	PIA2	READ THE PORT TO MAKE SURE THAT IT IS VALID
	BNE	LT12003_005
LT12001_009:	LDB	#$08	TIME TO WAIT
	STB	SW3SCI	SAVE IT
LT12005_001:	LDA	PIA2	READ THE PORT
	BNE	LT12002_005	BRA= STILL PRESSED
	CLR	SW2SCI	SAY THAT NO SWITCH HAS BEEN PRESSED
	RTS		TOO BAD YOU LET GO
LT12002_005:	CLRA		NAP ONLY 16 MS
	JSR	JNAP	AND TAKE A CAT NAP
	DEC	SW3SCI	ONE LESS TIME
	BNE	LT12005_001	NOT DONE SO CHECK AGAIN
	LDA	PIA2	READ ONCE AGAIN

LT12003_005:	PSHS	A
	TFR	U,X	GET THE CMOS LOCATION
	JSR	RCMSA
	PSHS	A
	CLRB
	STB	COLR
	BSR	OUTCMOS
	LDB	#WDATA
	STB	WDOG
 	LDX	#MINMAX	GET THE TABLE OF MAXIMUMS AND MINIMUMS
	TFR	U,D
	LEAX	B,X
	PULS	D
	ANDB	#$03	ONLY NEED BITS 0&1 (FOR JOUST)
	BITB	#$02	IS IT UP
	BNE	LT12010_001	BRA= NO
	BITB	#$01
	BEQ	CWRET
	CMPA	,X	IS IT AT IT'S MAX.
	BEQ	CWRET	BRA= YES..
	ADDA	#$99	BCD -1
	BRA	CMWRIT
LT12010_001:	CMPA	1,X	IS IT AT IT'S MIN.
	BEQ	CWRET	BRA= YES..
	ADDA	#$01	BCD +1

CMWRIT	DAA		MAKE IT DECIMAL AGAIN
	TFR	U,X
	JSR	WCMSA	WRITE THE NEW VALUE
CWRET	LDB	#$22
	STB	COLR
	JSR	OUTCMOS
	CLRA
	JMP	JNAP

MINMAX	FCB	$00,$99
	FCB	$01,$99
	FCB	$00,$01
	FCB	$00,$09
	FCB	$00,$99
	FCB	$00,$99
	FCB	$00,$99
	FCB	$01,$99
	FCB	$00,$99
	FCB	$00,$99
	FCB	$00,$09
	FCB	$03,$20
	FCB	$00,$01
	FCB	$00,$01
	FCB	$00,$01
	FCB	$00,$01
	FCB	$00,$01
	FCB	$00,$01

OUTCMOS	TFR	U,D
	LSRB
	LDX	#SPECAL
	LEAX	B,X
	TST	,X
	BMI	OUTSPC
OUT2	TFR	U,X
	JSR	RCMSA
	CMPU	#COINSL
	BNE	LT12002_006
	STA	SEED
	TST	BCDR
	BNE	LT12002_006
	BSR	PRICOUT
LT12002_006:	BITA	#$F0
	BNE	LT12001_010
	ORA	#$F0
LT12001_010:	PSHS	A
	TFR	Y,D
	LDA	#$6A
	TFR	D,X
	PULS	A
	LDB	COLR
	JSR	OUTB35
	RTS

OUTSPC	LDA	,X
	BITA	#$01
	BNE	LT12001_011
	TFR	U,X
	JSR	RCMSB
	LDA	#MSNO
	TSTB
	BEQ	LT12002_007
	LDA	#MSYES
LT12002_007:	PSHS	A
	TFR	Y,D
	LDA	#$6A
	TFR	D,X
	PULS	A
	LDB	COLR
	JMP	OUTP35
LT12001_011:	BSR	OUT2
	LDA	#CTHOU
	JMP	OUTC35

SPEC	EQU	$80
NOREPLC	EQU	$01
NORM	EQU	$00

SPECAL	FCB	SPEC+NOREPLC
	FCB	NORM
	FCB	SPEC
	FCB	NORM
	FCB	NORM
	FCB	NORM
	FCB	NORM
	FCB	NORM
	FCB	NORM
	FCB	NORM
	FCB	NORM
	FCB	NORM
	FCB	SPEC
	FCB	SPEC
	FCB	SPEC
	FCB	SPEC
	FCB	SPEC
	FCB	SPEC

PRICOUT	PSHS	D,U,X,Y
	LDX	#$003C			;#$0438!XDMAFIX
	STX	$CA06
	LDX	#$6A48
	STX	$CA04
	CLRB
	STB	$CA01
	LDB	#$12
	STB	$CA00
	LDX	#CSELCT
	CMPA	#9
	BNE	LT12008_000
	PSHS	A,X
	LDA	#MSFRPLY
	LDB	COLR
	LDX	#$703E
	JSR	OUTP35
	PULS	A,X
LT12008_000:	TFR	A,B
	ASLB
	PSHS	B
	ASLB
	ADDB	,S+
	ABX
	LEAU	2,U
LT12002_008:	EXG	U,X
	CMPX	#GA1	AT THE END OF COIN ADJUSTMENTS
	BEQ	LT12001_012
	LDA	,U+
	JSR	WCMSA
	LDB	#WDATA
	STB	WDOG
	EXG	U,X
	LEAY	$A,Y
	PSHS	X
	LEAX	-2,U
	JSR	RCMSA
	BITA	#$F0
	BNE	LT12010_002
	ORA	#$F0
LT12010_002:	PSHS	A
	TFR	Y,D
	LDA	#$6A
	TFR	D,X
	PULS	A
	LDB	COLR
	JSR	OUTB35
	PULS	X
	BRA	LT12002_008
LT12001_012:	PULS	D,U,X,Y,PC

*
* COINAGE SELECT TABLE
*
CSELCT	FCB	$01,$04,$01,$01,$00,$00 FACTORY USE FOR CUSTOM START
	FCB	$01,$04,$01,$02,$04,$00 50C..3/$1.00
	FCB	$06,$00,$01,$01,$00,$00 NEWNEW GERMAN
	FCB	$01,$04,$01,$01,$00,$00 25C
	FCB	$01,$16,$06,$02,$00,$00 NEW FRENCH
	FCB	$01,$04,$01,$02,$00,$00 50C
	FCB	$01,$00,$04,$01,$00,$00
	FCB	$01,$00,$02,$01,$00,$00
	FCB	$01,$00,$02,$02,$00,$00
	FCB	$00,$00,$00,$00,$00,$00 FREE PLAY
*
*
*	SOUND TEST
*

SNDSRT	JSR	SCCLR	CLEAR THE SCREEN
SNDSR1	LDD	#$FE01	SET UP FOR SOUND LINE 1
	STD	SW0SCN
	RTS

SNDCYC	LDA	#$3F
	STA	SOUND
	LDA	#3
	JSR	JNAP
SNDC55	CLRA
	STA	SOUND
	LDA	#3
	JSR	JNAP
SNDCY6	LDA	#$3F
	STA	SOUND
	LDA	#3
	JSR	JNAP
SNDCY1	LDD	SW0SCN
	ANDA	#$3F
	STA	SOUND
	LDB	#$99
	LDX	#$3A80
	LDA	#MSSND
	JSR	OUTPHR
	LDA	SW0SCN+1
	ORA	#$F0
	LDB	#$99
	JSR	OUTBCD
	LDA	#$40
	STA	SW0ST
SNDCY4	LDA	#$1
	JSR	JNAP
SNDCY5	JSR	AVCHK	ADVANCE???
	BCS	SNDCY3	THEN GO
	DEC	SW0ST
	BNE	SNDCY4
SNDCY3	LDA	FDTEST	SEE WHO SET THIS UP??
	BNE	SNDC90	AUTOCY DID....ALWAYS ADVANCE
	LDA	PIA0	CHECK MANUAL/AUTO
	RORA
	BCC	SNDC91	MANUAL...DON'T MOVE ON.
SNDC90	LDX	#$5780
	LDA	SW0SCN+1
	ORA	#$F0
	LDB	#$00
	JSR	OUTBCD
	LDD	SW0SCN
	ORCC	#$01	SEC
	ROLA		MOVE TO NEXT LINE
	INCB
	CMPB	#7	UP TO LINE 6??
	BLO	SNDCY2
	BSR	SNDSR1
SNDCY2	STD	SW0SCN
SNDC91	RTS


*
*	CMOS RAM TEST - We don't need this (save some space for our code), just clear the carry flag and return
*

CMTEST:
    ANDCC  #%11111110                 ; clear carry flag indicate CMOS RAM is good
    RTS
;	LDX	#CMOS
;	LDY	#LOWRAM	USE SAMRAM
;CMTST1	LDA	,X+	DON'T DO DOUBLES BECAUSE OF HARDWARE BRAIN DAM.
;	STA	,Y+
;	CMPX	#CMOS+$400 DONE??
;	BNE	CMTST1
;	LDB	#6	ENOUGH ITERATIONS TO ASSURE
;CMTST2	LDU	HSEED
;	LDY	SEED
;	LDX	#CMOS
;CMTST3	JSR	RAND_T12
;	STA	,X+
;	LDA	#WDATA
;	STA	WDOG
;	CMPX	#CMOS+$400 DONE??
;	BNE	CMTST3
;	STY	SEED	 RESTORE SEED
;	STU	HSEED
;	LDX	#CMOS
;CMTST4	JSR	RAND_T12
;	EORA	,X+
;	ANDA	#$F
;	BNE	CMEROR
;	LDA	#WDATA
;	STA	WDOG
;	CMPX	#CMOS+$400
;	BNE	CMTST4
;	DECB		ANOTHER PASS...DONE??
;	BNE	CMTST2
;	BSR	RAMBAK
;	ANDCC	#$FE	CLC CLEAR IT FOR REAL
;	RTS		AND RETURN

;RAMBAK	LDU	#LOWRAM
;	LDY	#CMOS
;RAMBK0	LDA	,U+
;	STA	,Y+
;	CMPY	#CMOS+$400
;	BNE	RAMBK0
;	RTS

;CMEROR	BSR	RAMBAK
;	ORCC	#$01	SEC
;	RTS
* End of CMOS Test code

*
*RANDOM NUMBER GENERATOR
*
RAND_T12	PSHS	B
	LDB	SEED
	LDA	#3
	MUL
	ADDB	#17
	LDA	LSEED
	LSRA
	LSRA
	LSRA
	EORA	LSEED
	LSRA
	ROR	HSEED
	ROR	LSEED
	ADDB	LSEED
	ADCB	HSEED
	STB	SEED
	LDA	SEED
	PULS	B,PC

*
*	AND THE EVER POPULAR COPYRIGHT MESSAGE

;	FCC	' JOUST - COPYRIGHT  (C) 1982 WILLIAMS ELECTRONICS INC. '
;	FCC	' ALL RIGHTS RESERVED '

IRQVEC	JMP	[$EFF8]


CYCLE	LDA	#PRAM
	LDX	#CLRTAB
	LDB	,X
	STB	PIMAGE,U
LT12001_013:	LDB	,X+
	STB	A,U
	INCA
	CMPA	#PRAM+8
	BNE	LT12001_013
	CLRA
	STA	PTIMX,U
	LEAY	PRAM+7,U
	STY	PDIST,U
LT12002_009:	PCNAP	4
	LDB	#PRAM
	ADDB	PTIMX,U
	LDA	PIMAGE,U
	STA	B,U
	LDA	PTIMX,U
	INCA
	CMPA	#$08
	BNE	LT12004_002
	CLRA
LT12004_002:	CMPA	#$04
	BNE	LT12007_000
	LEAX	PRAM,U
	LDB	,X+
	PSHS	B
LT12006_000:	LDB	,X+
	STB	-2,X
	CMPX	PDIST,U
	BLE	LT12006_000
	PULS	B
	STB	-1,X
LT12007_000:	STA	PTIMX,U
	ADDA	#PRAM
	LDB	A,U
	STB	PIMAGE,U
RGB_Monitor_Value:
	 LDB	#$19			* Extra colour inserted in the palette for the highscore border Joust $E8 = CoCo3 RGB colour $19
  STB	A,U
	LDX	#RAMCOL+8
	LEAY	PRAM,U
LT12003_006:	LDA	,Y+
	STA	,X+
	CMPX	#RAMCOL+15
	BLE	LT12003_006
	BRA	LT12002_009

* Draws a border around the screen $FF87
TEST:
    LDD     #$0001
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4

	LDA	#$77				* Starting colour pattern for the blocks around the border of the screen
	STA	XTEMP				* Seems to be used temporarily for counters and pointers MSB
	LDX	#$912				* Destination address on screen (left 9 bytes (18 pixels) down 12 rows)
LT12001_014:
	BSR	SUB1				* Draw a 6x8 block on screen
	LEAX	$600,X		* X=X+$600, move right 6 bytes
	CMPX	#$8D00		* Are we at the right of the screen yet?
	BLO	LT12001_014	* If not draw another block
LT12002_010:
	BSR	SUB2				* Draw a 4x12 block on screen
	LEAX	$0C,X			* X=X+12 (move down rows)
	CMPX	#$8DE8		* Are we at the bottom right corner yet?
	BLO	LT12002_010	* Loop again if not
	LEAX	-$608,X		* X=X-$608
LT12003_007:
	BSR	SUB1				* Draw a 6x8 block on screen
	CMPX	#$9E3			* Are we at the left edge of the screen? If Not Carry bit=0
	LEAX	-$600,X		* X=X-$600
	BHI	LT12003_007	* if Z=0 and C=0 then loop
	LEAX	+$1FC,X		* X=X+$1FC
LT12004_003:
	BSR	SUB2				* Draw a 4x12 block on screen
	LEAX	-$0C,X		* X=X-12 (move up 12 rows)
	CMPX	#$50E			* Are we at the top left corner?
	BHI	LT12004_003	* Loop if not

* We need to restore MMU Blocks 0 to 4 banks to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4

	RTS							* Done border, return


;SUB2	LDD	#$0008			;#$040C!XDMAFIX
;	BRA	SUB3
;SUB1	LDD	#$020C			;#$0608!XDMAFIX
;SUB3	STD	$CA06
;	STX	$CA04
;	LDA	XTEMP
;	ADDA	#$11
;	BCC	LT12001_015
;	LDA	#$88
;LT12001_015:	STA	XTEMP
;	STA	$CA01
;	LDA	#$12
;	STA	$CA00
;	RTS
* My nots and code to handle the above on the CoCo 3
;------------------------------------
;_Branch_FFBF:
;    LDD      #$0008                               ;FFBF    CC0008       * Blitter Width is 4 = 0000 0000 XORd with 4 = 0000 0100, Height 12 = 8 XORd with 4
;    BRA      _Branch_FFC7                         ;FFC2    2003         * Skip ahead
;_Branch_FFC4:
;    LDD      #$020C                               ;FFC4    CC020C       * Blitter Width = 6 = 2 XORd 4, Height 8 = $0C XORd with 4 (0000 1100 becomes 0000 1000)
;_Branch_FFC7:
;    STD      Blitter_Width                        ;FFC7    FDCA06       Width of sprite for blitter
;    STX      Blitter_DestinationAddress           ;FFCA    BFCA04       Destination address on screen to draw blitter
;    LDA      <TempVariableMSB_A0D5                ;FFCD    96D5         Seems to be used temporarily for counters and pointers MSB
;    ADDA     #$11            ; Bits: 0001 0001    ;FFCF    8B11         * add $11 so cycle through $88,$99,$AA,$BB,$CC,$DD,$EE,$FF
;    BCC      _Branch_FFD5                         ;FFD1    2402         * if we aren't greater than $FF then skip ahead
;    LDA      #$88            ; Bits: 1000 1000    ;FFD3    8688         * If we get to $00 then make it $88
;_Branch_FFD5:
;    STA      <TempVariableMSB_A0D5                ;FFD5    97D5         Seems to be used temporarily for counters and pointers MSB
;    STA      Blitter_Mask                         ;FFD7    B7CA01       Blitter Mask
;    LDA      #$12            ; Bits: 0001 0010    ;FFDA    8612         * solid mode (doesn't use a source address) + synchronise with E clock (for RAM to RAM blits)
;    STA      Blitter_Execute                      ;FFDC    B7CA00       Run the Blitter (update sprite on screen)
;    RTS                                           ;FFDF    39
* Draw a rectangle that is 4 x 12
_Branch_FFBF:
SUB2:
    LDA      <XTEMP             								  ;FFCD    96D5         Seems to be used temporarily for counters and pointers MSB
    ADDA     #$11            ; Bits: 0001 0001    ;FFCF    8B11         * add $11 so cycle through $88,$99,$AA,$BB,$CC,$DD,$EE,$FF
    BCC      >                                    ;FFD1    2402         * if we aren't greater than $FF then skip ahead
    LDA      #$88            ; Bits: 1000 1000    ;FFD3    8688         * If we get to $00 then make it $88
!   STA      <XTEMP                								;FFD5    97D5         Seems to be used temporarily for counters and pointers MSB
    TFR     A,B
    PSHS    D,X,U             * ,S=Solid Colour 2,S=Joust Destination address
    LDD     2,S               * D= the starting address
* Change Joust Screen location to CoCo3 value in D
    STA     SelfModBorder1+2
    LDA     #160              * 160 bytes per row
    MUL
SelfModBorder1:
    ADDD    #$0000            * D now has the CoCo 3 screen location to start the block
    TFR     D,X
    LDD     #12*$100+160      * A=12 rows to draw and B = 160 bytes per row
    LDU     ,S                * U = the solid colour for this square
!   STU     ,X
    STU     2,X
    ABX                       * Move down a row
    DECA                      * decrement counter
    BNE     <                 * Not done then loop
    PULS    D,X,U,PC

* Draw a rectangle that is 6 x 8
_Branch_FFC4:
SUB1:
    LDA      <XTEMP                								;FFCD    96D5         Seems to be used temporarily for counters and pointers MSB
    ADDA     #$11            ; Bits: 0001 0001    ;FFCF    8B11         * add $11 so cycle through $88,$99,$AA,$BB,$CC,$DD,$EE,$FF
    BCC      >                                    ;FFD1    2402         * if we aren't greater than $FF then skip ahead
    LDA      #$88            ; Bits: 1000 1000    ;FFD3    8688         * If we get to $00 then make it $88
!   STA      <XTEMP                								;FFD5    97D5         Seems to be used temporarily for counters and pointers MSB
    TFR     A,B
    PSHS    D,X,U             * ,S=Solid Colour 2,S=Joust Destination address
    LDD     2,S               * D= the starting address
* Change Joust Screen location to CoCo3 value in D
    STA     SelfModBorder2+2
    LDA     #160              * 160 bytes per row
    MUL
SelfModBorder2:
    ADDD    #$0000            * D now has the CoCo 3 screen location to start the block
    TFR     D,X
    LDD     #8*$100+160      * A=8 rows to draw and B = 160 bytes per row
    LDU     ,S                * U = the solid colour for this square
!   STU     ,X
    STU     2,X
    STU     4,X
    ABX                       * Move down a row
    DECA                      * decrement counter
    BNE     <                 * Not done then loop
    PULS    D,X,U,PC
;------------------------------------

CLRTAB:
PaletteData_Highscores:                           ;FFE0       xxRGBrgb  BbGGgRRr
    FCB    $3C  ;$AF                              ;FFE0				00111100  10101111
    FCB    $37  ;$77                              ;FFE1				00110111  01110111
    FCB    $36  ;$37                              ;FFE2       00110110  00110111
    FCB    $26  ;$1F                              ;FFE3       00100110  00011111
    FCB    $26  ;$17                              ;FFE4				00100110  00010111
    FCB    $24  ;$0F                              ;FFE5				00100100	00001111
    FCB    $20  ;$04                              ;FFE6				00100000	00000100
    FCB    $04  ;$0A                              ;FFE7				00000100	00001010
;CLRTAB	FCB	$AF
;	FCB	$77
;	FCB	$37
;	FCB	$1F
;	FCB	$17
;	FCB	$0F
;	FCB	$04
;	FCB	$0A

*
ENDADR_T12REV3.SRC.out	EQU	*
LENGTH_T12REV3.SRC.out	EQU	ENDADR_T12REV3.SRC.out-TSTORG
*
;	ORG	TSTORG+$FF0
;	FDB	PWRUPV,PWRUPV,PWRUPV,PWRUPV
;	FDB	IRQVEC,PWRUPV,PWRUPV,PWRUPV
*

;------------------------------------
* Clear the screen, stack blast zeros on screen from $9FFF to $0000
Clear_ScreenJump_JSR_3B2E:
;    JMP      Clear_Screen                         ;3B2E    7E40BC       Clear the screen, stack blast zeros on screen from $9FFF to $0000
Clear_Screen_JMP_40BC:        ; 40BC
Clear_ScreenJump:
Clear_Screen:                                     * Clear Screen RAM from $0000-$9FFF
SCCLER_TB12REV3:
    PSHS  D,X,Y,U
* Handle screen MMU Mapping
* We use VideoRam0_0 = Bank 0 to VideoRam0_4 = Bank 4
* Screen RAM is from $0000-$9FFF
* We need to setup MMU Blocks 0 to 4 to screen banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4
    LDD     #$0001
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
		DECB		* Make D = $0000
;		LDD		#$0000  * D is already 0
		LDX		#$0000
		LEAY	,X
;		LDU		#$8CA0
    LDU   #$A000
!		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
    PSHU    D,X,Y
		PSHU    D,X,Y		* 16 * 6 = 96
		CMPU	#$0040    * = 64   Loop until we are almost done then do the last bit below
		BHI		<
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
		PSHU    D,X,Y
    PSHU    D,X,Y
		PSHU    D,X,Y		* 10 * 6 = 36
    PSHU    D,X     * + 4 = 64  Done clearing the screen

* We need to restore MMU Blocks 0 to 4 banks to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4
    PULS    A,B,X,Y,U,PC                         ;40DF    35F6
;------------------------------------ TOK



*
* CMOS PRIMITIVE FOR READING
*
RCMOSA_TB12REV3:	EQU	*
RCMOS_TB12REV3:
* Read from CMOS
Read_CMOS_BYTES_XINTO_A_JSR_3B31:
;    JMP      Read_CMOS_BYTES_XINTO_A              ;3B31    7E40E1       Read 2 CMOS bytes pointed to by X in A and X=X+2
Read_CMOS_BYTES_XINTO_A_JMP_40E1:
Read_CMOS_BYTES_XINTO_A:
    PSHS    B
    LDB     #CMOS_Storage
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) to CMOS Bank
    LDA      +1,X                                 ;40E1    A601         *  read number from X + 1
    ANDA     #$0F            ; Bits: 0000 1111    ;40E3    840F         * Keep only right nibble
    PSHS     A                                    ;40E5    3402         * save result on stack to be used @ 40ED
    LDA      ,X++                                 ;40E7    A681         * read number @ X, then X=X=2
    ASLA                                          ;40E9    48
    ASLA                                          ;40EA    48
    ASLA                                          ;40EB    48
    ASLA                                          ;40EC    48           * Move right nibble to left nibble
    ADDA     ,S+                                  ;40ED    ABE0         * add byte above to current A and fix Stack pointer
    LDB     #RegRAM6
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) back to normal mode
    PULS    B,PC                                      ;40EF    39
;------------------------------------
*
* WRITE TO CMOS PRIMITIVE
*
WCMOSA_TB12REV3:	EQU	*
WCMOS_TB12REV3:
* Write to CMOS
Write_CMOS_A_INTO_X_JSR_3B3A:
;    JMP      Write_CMOS_A_INTO_X                  ;3B3A    7E40FA       Write A into CMOS bytes pointed to by X and X=X+2
Write_CMOS_A_INTO_X_JMP_40FA:
Write_CMOS_A_INTO_X:
    PSHS    A,B
    LDB     #CMOS_Storage
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) to CMOS Bank
;    PSHS     A                                    ;40FA    3402
    STA      +1,X                                 ;40FC    A701         * Save A 1,X
    LSRA                                          ;40FE    44
    LSRA                                          ;40FF    44
    LSRA                                          ;4100    44
    LSRA                                          ;4101    44           * left nibble is now in the right nibble
    STA      ,X++                                 ;4102    A781         * SAVE A @ X and X=X+2
    LDB     #RegRAM6
    STB     MMU_Reg_Bank0_6                       * Change MMU bank 6 (C000-DFFF) back to normal mode
    PULS     A,B,PC                                 ;4104    3582
;------------------------------------




;------------------------------------
CharacterTableAddress:                            ;4A68                 Points to the character table
    FDB      FONT5                                ;4A68
;------------------------------------
Write_SmallFontCharAatX_JSR_4A59:
Write_SmallFontCharAatX_JMP_4A70:
Write_SmallFontCharAatX:
CHR35:
    PSHS     CC,A,B,Y,U                           ;4A70    3467         * Write character in A on screen at X using the large font and X=X+width of letter printed
    ORCC     #$FF    ; CC Flags: EFHINZVC         ;4A76    1AFF         * Disable the Interrupts
    STB     Blitter_Mask
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDD			#$0400+FontPalette00
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
* Setup the correct block to draw the font in the correct color based on blitter mask value
		STB			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table
		LDA			1,S								* restore A which points to the character

    LDY      #CharacterSmallFontTable             ;4A72    108E4BC6     Table pointers to Small font characters * Y points to the small Font Table
;    STB      Blitter_Mask                         ;4A78    F7CA01       Blitter Mask * update the blitter mask with the value in B
    BSR      Write_Character_On_Screen            ;4A7B    8D11         * User Blitter to write character in A on screen at position X, X=X+width of character

* Put blocksback to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    LDD			#$3C00+RegRAM6		 * Memory block $3E
    STA     MMU_Reg_Bank0_4  * Set Bank 4
		STB			MMU_Reg_Bank0_6	 * Page $C000-$DFFF  Bank #6

    PULS     CC,A,B,Y,U,PC                        ;4A7D    35E7
;------------------------------------
Write_LargeFontCharAatX_JMP_4A7F:
Write_LargeFontCharAatX_JSR_4A50:
Write_LargeFontCharAatX:
CHROUT:
    PSHS     CC,A,B,Y,U                           ;4A7F    3467         * Write character in A on screen at X using the large font and X=X+width of letter printed
		ORCC     #$FF    ; CC Flags: EFHINZVC         ;4A85    1AFF         * Disable the Interrupts
    STB     Blitter_Mask
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDD			#$0400+FontPalette00
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
* Setup the correct block to draw the font in the correct color based on blitter mask value
		STB			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table
		LDA			1,S								* restore A which points to the character

    LDY      #CharacterBigFontTable               ;4A81    108E4B5C     Table pointers to Big font characters * Y points to the big Font Table
;    STB      Blitter_Mask                         ;4A87    F7CA01       Blitter Mask * update the blitter mask with the value in B
    BSR      Write_Character_On_Screen            ;4A8A    8D02         * User Blitter to write character in A on screen at position X, X=X+width of character

* Put blocksback to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    LDD			#$3C00+RegRAM6		 * Memory block $3E
    STA     MMU_Reg_Bank0_4  * Set Bank 4
		STB			MMU_Reg_Bank0_6	 * Page $C000-$DFFF  Bank #6

    PULS     CC,A,B,Y,U,PC                        ;4A8C    35E7
;------------------------------------
* Write Character A on screen at mem location in X, X=X+ width of the character drawn
Write_Character_On_Screen_Branch_4A8E:
Write_Character_On_Screen:
CoCo3_Write_CharAatX:
OUTPUT:
    LDB     #4                                    * A = character number
    ANDA    #%01111111                            * Strip off bit 7 just in case it flags the last byte of the messages
    MUL                                           * D = Character # * 4 (4 bytes per table entry, first two are the width and height, 2nd two are the jump address to compiled sprite)
    LEAY    D,Y                                   * Y points to the proper location in the table
    LDA     ,Y++                                  * Get the character width in pixels
    CLRB                                          * Don't want to add anything to the Y value (height)
    STX     SelfModFont1+1     * Self modifying code, save current starting address
    LEAX    D,X               * X now points to the address where the following byte will be displayed

* Setup the correct block to draw the font in the correct color based on blitter mask value
    LDB     Blitter_Mask
    ANDB    #%00001111        * Make sure it's only 0 to 15
;    BNE     >                 * Should never = 0 unless it really draws black text ???
;    LDB     #1                * Testing use 1 for zero for now
;!
    ADDB    #FontPalette00
    STB     MMU_Reg_Bank0_6   * Setup block 6 with the correct colour font compiled sprite code and lookup table

SelfModFont1:
    LDD     #$FFFF            * Self mod code D= the starting address
* Change Joust Screen location to CoCo3 value in D
    STA     SelfModFont2+2
    LDA     #160              * 160 bytes per row
    MUL
SelfModFont2:
    ADDD    #$0000
    JSR     [,Y]              * Go draw character on screen
    RTS
;------------------------------------
Write_SmallMessage_AatX_JMP_4AAD:
Write_SmallMessage_AatX_JSR_4A5C:
Write_SmallMessage_AatX:
PHR35:
    PSHS     CC,A,B,Y,U                           ;4AAD    3467         * Save the registers except X

    ORCC     #$FF    ; CC Flags: EFHINZVC         ;4AB6    1AFF         * Disable the Interrupts
    STB     Blitter_Mask
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDD			#$0400+FontPalette00
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
* Setup the correct block to draw the font in the correct color based on blitter mask value
		STB			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table
		LDA			1,S								* restore A which points to the character

    LDY      #CharacterSmallFontTable             ;4AAF    108E4BC6     Table pointers to Small font characters * Y points to the small Font Table
    STY      <XSAVE                               ;4AB3    109FD1       * Save Y temporarily in $D1
;    STB      Blitter_Mask                        ;4AB8    F7CA01       Blitter Mask * update the blitter mask with the value in B
    BSR      _Branch_4AD1                         ;4ABB    8D14         * Go print message on screen bit 7 of character will signal end of message
    PULS     CC,A,B,Y,U,PC                        ;4ABD    35E7
;------------------------------------
Write_BigMessage_AatX_JMP_4ABF:
Write_BigMessage_AatX_JSR_4A53:
Write_BigMessage_AatX:
PHROUT:
    PSHS     CC,A,B,Y,U                           ;4ABF    3467         * Save the registers except X

    ORCC     #$FF    ; CC Flags: EFHINZVC         ;4AC8    1AFF         * Disable the Interrupts
    STB     Blitter_Mask
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDD			#$0400+FontPalette00
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
* Setup the correct block to draw the font in the correct color based on blitter mask value
		STB			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table
		LDA			1,S								* restore A which points to the character

    LDY      #FontTable                           ;4AC1    108E4B5C     Table pointers to Big font characters * Y points to the big Font Table
    STY      <XSAVE                               ;4AC5    109FD1       * Save Y to address in <$D1
;    STB      Blitter_Mask                        ;4ACA    F7CA01       Blitter Mask * update the blitter mask with the value in B
    BSR      _Branch_4AD1                         ;4ACD    8D02         * Go print message on screen bit 7 of character will signal end of message
    PULS     CC,A,B,Y,U,PC
;------------------------------------
_Branch_4AD1:
PHROT1:
    LDU      #Message_Table_Start                 ;4AD1    CE531F       00-THY GAME IS OVER * Y now points to the address of the first entry to the message table
    LEAU     A,U                                  ;4AD4    33C6         * U=U+A
    LDU      A,U                                  ;4AD6    EEC6         * A is double and U is loaded with the address of the start of the character data (Width & Height)
_Branch_4AD8:
    LDA      ,U                                   ;4AD8    A6C4         * A = character to print on screen
    LDY      <XSAVE                               ;4ADA    109ED1       *  restore Y with old value which is the pointer to the start of this table
    JSR      CoCo3_Write_CharAatX                 ;4ADD    BD4A8E       * Write A at X, X=X+width of character
    TST      ,U+                                  ;4AE0    6DC0         * Test if we reached the end of the text message, move U to the next letter in message
    BPL      _Branch_4AD8                         ;4AE2    2AF4         * if bit 7 is not set then go print next chacter in message to screen


* Put blocksback to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    LDD			#$3C00+RegRAM6		 * Memory block $3E
    STA     MMU_Reg_Bank0_4  * Set Bank 4
		STB			MMU_Reg_Bank0_6	 * Page $C000-$DFFF  Bank #6

    RTS
;------------------------------------
Print_BCD_Number_Small_B_Mask_JMP_4AE5:
Print_BCD_Number_Small_B_Mask:
BCD35:
    PSHS     CC,A,B,Y,U                           ;4AE5    3467

    ORCC     #$FF    ; CC Flags: EFHINZVC         ;4AEE    1AFF         * Disable the Interrupts
    STB     Blitter_Mask
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDD			#$0400+FontPalette00
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
* Setup the correct block to draw the font in the correct color based on blitter mask value
		STB			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table
		LDA			1,S								* restore A which points to the character

    LDY      #CharacterSmallFontTable             ;4AE7    108E4BC6     Table pointers to Small font characters * Y points to the small Font Table
    STY      <XSAVE                               ;4AEB    109FD1       * Save Y to address in <$D1
;    STB      Blitter_Mask                        ;4AF0    F7CA01       Blitter Mask * update the blitter mask with the value in B
    BSR      _Branch_4B09                         ;4AF3    8D14         * Go print two BCD numbers in A on screen
    PULS     CC,A,B,Y,U,PC                        ;4AF5    35E7
;------------------------------------
Print_BCD_Number_Big_B_Mask_JSR_4A56:
Print_BCD_Number_Big_B_Mask:
BCDOUT:
    PSHS     CC,A,B,Y,U                           ;4AF7    3467

    ORCC     #$FF    ; CC Flags: EFHINZVC         ;4B00    1AFF         * Disable the Interrupts
    STB     Blitter_Mask
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDD			#$0400+FontPalette00
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
* Setup the correct block to draw the font in the correct color based on blitter mask value
		STB			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table
		LDA			1,S								* restore A which points to the character

    LDY      #CharacterBigFontTable               ;4AF9    108E4B5C     Table pointers to Big font characters * Y points to the big Font Table
    STY      <XSAVE              							    ;4AFD    109FD1       * Save Y to address in <$D1
;    STB      Blitter_Mask                         ;4B02    F7CA01       Blitter Mask * update the blitter mask with the value in B
    BSR      _Branch_4B09                         ;4B05    8D02         * Go print two BCD numbers in A on screen
    PULS     CC,A,B,Y,U,PC                        ;4B07    35E7
;------------------------------------
_Branch_4B09:
BCDOT1:
    STA      <ASAVE		                            ;4B09    97D0         * Save A
    LSRA                                          ;4B0B    44
    LSRA                                          ;4B0C    44
    LSRA                                          ;4B0D    44
    LSRA                                          ;4B0E    44           * above move a left nibble to the right (first digit to print on screen)
    CMPA     #$0A            ; Bits: 0000 1010    ;4B0F    810A         * Check if A <= 10
    BLE      _Branch_4B15                         ;4B11    2F02         * Skip if true
    LDA      #$0A            ; Bits: 0000 1010    ;4B13    860A         * otherwise A=10 (10 is a space)
_Branch_4B15:
    LDY      <XSAVE      									        ;4B15    109ED1       * restore Y address so it points to the start of the font table
    JSR      Write_Character_On_Screen            ;4B18    BD4A8E       * Go write number in A on screen at X
    LDA      <ASAVE      			                    ;4B1B    96D0         * Retore A
    ANDA     #$0F            ; Bits: 0000 1111    ;4B1D    840F         * Only need the left nibble (2nd number to print to screen)
    CMPA     #$0A            ; Bits: 0000 1010    ;4B1F    810A         * Check if A <= 10
    BLE      _Branch_4B25                         ;4B21    2F02         * Skip if true
    LDA      #$0A            ; Bits: 0000 1010    ;4B23    860A         * otherwise A=10 (10 is a space)
_Branch_4B25:
    LDY      <XSAVE              								  ;4B25    109ED1       * restore Y address so it points to the start of the font table
    JSR      Write_Character_On_Screen            ;4B28    7E4A8E       * Go write number in A on screen at X and return

* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    LDD			#$3C00+RegRAM6	 * A=$3C, B=Memory block $3E
    STA     MMU_Reg_Bank0_4  * Set Bank 4
		STB			MMU_Reg_Bank0_6	 * Page $C000-$DFFF  Bank #6

    RTS
;------------------------------------
Write_MessagesSmallNoMask_JSR_4A6D:
Write_MessagesSmallNoMask_JMP_4B2B:
Write_MessagesSmallNoMask:
ERTT35:
    INC      <FLAG                                ;4B2B    0CF6         * Make flag non-zero = masking of $00
    BRA      _Branch_4B31                         ;4B2D    2002         * Skip ahead
;------------------------------------
Write_MessagesSmallKeepColour_JMP_4A65:
Write_MessagesSmallKeepColour_JMP_4B2F:
Write_MessagesSmallKeepColour:
OUTT35:
    CLR      <FLAG                                ;4B2F    0FF6         * Make flag 0 - Keep colour of mask
_Branch_4B31:
OUT35A:
    LDU      #Write_SmallMessage_AatX             ;4B31    CE4A5C       * Set U to point to the write small message routine
    BRA      _Branch_4B3F                         ;4B34    2009         * Skip forward
;------------------------------------
Write_MessagesBigNoMask_JSR_4A6A:
Write_MessagesBigNoMask_JMP_4B36:
Write_MessagesBigNoMask:
ERTEXT:
    INC      <FLAG                                ;4B36    0CF6         * Make flag non-zero = masking of $00
    BRA      _Branch_4B3C                         ;4B38    2002         * Skip ahead
;------------------------------------
Write_MessagesBigKeepColour_JMP_4B3A:
Write_MessagesBigKeepColour_JSR_4A62:
Write_MessagesBigKeepColour:
OUTTEXT:
    CLR      <FLAG                                ;4B3A    0FF6         * Make flag 0 - Keep colour of mask
_Branch_4B3C:
OUTTX2:
    LDU      #Write_BigMessage_AatX               ;4B3C    CE4A53       * Set U to point to the write big message routine
_Branch_4B3F:
OUTTX1:
* Setup the correct block to draw the font in the correct color based on blitter mask value
    LDB     #FontPalette00
    STB     MMU_Reg_Bank0_6   * Setup block 6 with the correct colour font compiled sprite code and lookup table

    LDY      #MultiMessagePointer                 ;4B3F    108E5D7B     MultiMessagePointer * Y = MultiMessagePointer
    LEAY     A,Y                                  ;4B43    31A6         * Y=Y+A
    LDY      A,Y                                  ;4B45    10AEA6       * A now points to the multi-message, Y now points to the start of the bunch of messages
_Branch_4B48:
    LDX      ,Y++                                 ;4B48    AEA1         * Get screen location
    LDB      ,Y+                                  ;4B4A    E6A0         * Get colour mask
    TST      <FLAG                                ;4B4C    0DF6         * Test if masking flag = 0
    BEQ      _Branch_4B51                         ;4B4E    2701         * if yes skip ahead
    CLRB                                          ;4B50    5F           * Otherwise Mask = 0
_Branch_4B51:
    LDA      ,Y                                   ;4B51    A6A4         * Get message #
    ANDA     #$7F            ; Bits: 0111 1111    ;4B53    847F         * make it not negative
    JSR      ,U                                   ;4B55    ADC4         * Write message A at X (could be small message or big message)

* After the 1st message is printed MMU bank 6 will be set back to normal, we still need it to be in Font mode
* Setup the correct block to draw the font in the correct color based on blitter mask value
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDD			#$0400+FontPalette00
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
* Setup the correct block to draw the font in the correct color based on blitter mask value
		STB			MMU_Reg_Bank0_6	 	* Setup block 6 with the correct colour font compiled sprite code and lookup table

    TST      ,Y+                                  ;4B57    6DA0         * is this the last message to print?
    BPL      _Branch_4B48                         ;4B59    2AED         * If negative then RTS else loop and print next message

* All done with messages
* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    LDD			#$3C00+RegRAM6	 * A=$3C, B=Memory block $3E
    STA     MMU_Reg_Bank0_4  * Set Bank 4
		STB			MMU_Reg_Bank0_6	 * Page $C000-$DFFF  Bank #6

    RTS                                           ;4B5B    39
;------------------------------------
* Draw box on screen for the word CREDITS to be drawn over 27 wide & 13 High (maybe really should be 31 wide x 9 high)
* LSB of Y = Palette# for box colour
Do_Blitter_01_JSR_8793:
Do_Blitter_01:
    PSHS    U
* Change Joust Screen location to CoCo3 value in D
    STA     SelfModDoBlit1+2
    LDA     #160              * 160 bytes per row
    MUL
SelfModDoBlit1:
    ADDD    #$0000
    TFR     D,U

* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    INCB
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank

    STU     BoxLocation       * save the location of the start of the CREDITS box, so 2nd attract mode can make the palette 0 palette 1 (so border will ignore it)
    LEAU    -160,U            * U now a row above where we need to be and the left side
    LDA     #9                * Number of rows to draw
    STA     BlitRows
    TFR     Y,D               * B now = Palette pattern for the pixels of the box
    TFR     B,A               * D now = ""
    TFR     D,X               * X now = ""
    TFR     D,Y               * Y now = ""
!   LEAU    160+30,U          * Move down a row and to the right edge of the box
    STA     ,U                * 1 byte
    PSHU    D,X,Y             * 7 bytes
    PSHU    D,X,Y             * 13 bytes
    PSHU    D,X,Y             * 19 bytes
    PSHU    D,X,Y             * 25 bytes
    PSHU    D,X,Y             * 31 bytes - One row is done
    DEC     BlitRows
    BNE     <
* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
		INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4
    PULS    U,PC

* Patch credits box to looks correct in the Attract screen "Big Logo" around $D000-
* Need to fix the Credits box so the black which is drawn as palette 0 needs to be palette 1
* size of 31 wide x 9 high (really 29 x 7)
* BoxLocation = top left corner of the box to fix
GlenPatchCredits:
    PSHS    D,U   * Save the registers
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    INCB
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank

    LDU     BoxLocation
    LEAU    161,U
    LDA     #7       * Number of rows high
    STA     BlitRows * Save it for counter
FixBlack0:
    LDB     #29
FixBlack:
    LDA     ,U
    BEQ     >       * Both nibbles are palette 0
    CMPA    #$22
    BEQ     FixBlackUpdate
    CMPA    #$20    * Test left nibble
    BEQ     FixBlack1 * IF left nibble <> 0 then skip ahead
    LDA     #$12      * Make left nibble Pallete 1
    BRA     FixBlackUpdate
FixBlack1:
    LDA     #$21      * Right nibble must be palette 0 so make it Palette 1
    BRA     FixBlackUpdate
!
    LDA     #$11    * Both nibbles are now Palette 1
FixBlackUpdate:
    STA     ,U+
    DECB            * Have we done 31 bytes yet?
    BNE     FixBlack  * If not keep going
    LEAU    160-29,U  * move down a line and to the left of the box
    DEC     BlitRows  * decrement row counter
    BNE     FixBlack0
* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
		INCB
    STB     MMU_Reg_Bank0_4  * Set Bank 4
    PULS    D,U,PC           * All done restore and return


* This is the start of the 3rd attract screen with the Demo/simulation
* moved from lower RAM $5EEB to here, then we will change the MMU Block to
*
*	GAME SIMULATION OR INSTRUCTIONAL PAGE
*
;GAMSIM:
;* Set lower RAM to screen mode and $C000 to the Demo/simulation code block
;		LDD			#$0001
;    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
;    LDD     #$0203
;    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
;    LDD     #$0400+GameSimulation
;    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
;    STB     MMU_Reg_Bank0_6   * Set bank 6 to the Demo/simulation code
;
;	LDA	#$7F		GAME SIMULATION MODE
;	STA	GOVER
;	LDA	#3		3 LIVES FOR EACH PLAYER
;	STA	NLIVES
;	STA	NPLYRS		& 2 PLAYERS
;	JMP	SIM1A		START THE PAGE

;* Load the Game simulation code for attract screen 3 into $C000-$DFFF MMU block 6
;  ORG   MMU_Reg_Bank0_6
;  FCB   GameSimulation
;  ORG   $C000
;  INCLUDE  ./Joust_CoCo3_GameSimCode.asm


PaletteSlot2_Lookup
    FCB     $00
* Lookup table using B,X as a pointer B is a signed value so we account for that by moving the data into negative values and positive values
* (pointer starts in the middle of the table)
    FCB    $08  ;$80
    FCB    $08  ;$81
    FCB    $0C  ;$82
    FCB    $0C  ;$83
    FCB    $28  ;$84
    FCB    $28  ;$85
    FCB    $2C  ;$86
    FCB    $2C  ;$87
    FCB    $08  ;$88
    FCB    $08  ;$89
    FCB    $0C  ;$8A
    FCB    $0C  ;$8B
    FCB    $28  ;$8C
    FCB    $28  ;$8D
    FCB    $2C  ;$8E
    FCB    $2C  ;$8F
    FCB    $0A  ;$90
    FCB    $0A  ;$91
    FCB    $0E  ;$92
    FCB    $0E  ;$93
    FCB    $2A  ;$94
    FCB    $2A  ;$95
    FCB    $2E  ;$96
    FCB    $2E  ;$97
    FCB    $0A  ;$98
    FCB    $0A  ;$99
    FCB    $0E  ;$9A
    FCB    $0E  ;$9B
    FCB    $2A  ;$9C
    FCB    $2A  ;$9D
    FCB    $2E  ;$9E
    FCB    $2E  ;$9F
    FCB    $18  ;$A0
    FCB    $18  ;$A1
    FCB    $1C  ;$A2
    FCB    $1C  ;$A3
    FCB    $38  ;$A4
    FCB    $38  ;$A5
    FCB    $3C  ;$A6
    FCB    $3C  ;$A7
    FCB    $18  ;$A8
    FCB    $18  ;$A9
    FCB    $1C  ;$AA
    FCB    $1C  ;$AB
    FCB    $38  ;$AC
    FCB    $38  ;$AD
    FCB    $3C  ;$AE
    FCB    $3C  ;$AF
    FCB    $1A  ;$B0
    FCB    $1A  ;$B1
    FCB    $1E  ;$B2
    FCB    $1E  ;$B3
    FCB    $3A  ;$B4
    FCB    $3A  ;$B5
    FCB    $3E  ;$B6
    FCB    $3E  ;$B7
    FCB    $1A  ;$B8
    FCB    $1A  ;$B9
    FCB    $1E  ;$BA
    FCB    $1E  ;$BB
    FCB    $3A  ;$BC
    FCB    $3A  ;$BD
    FCB    $3E  ;$BE
    FCB    $3E  ;$BF
    FCB    $09  ;$C0
    FCB    $09  ;$C1
    FCB    $0D  ;$C2
    FCB    $0D  ;$C3
    FCB    $29  ;$C4
    FCB    $29  ;$C5
    FCB    $2D  ;$C6
    FCB    $2D  ;$C7
    FCB    $09  ;$C8
    FCB    $09  ;$C9
    FCB    $0D  ;$CA
    FCB    $0D  ;$CB
    FCB    $29  ;$CC
    FCB    $29  ;$CD
    FCB    $2D  ;$CE
    FCB    $2D  ;$CF
    FCB    $0B  ;$D0
    FCB    $0B  ;$D1
    FCB    $0F  ;$D2
    FCB    $0F  ;$D3
    FCB    $2B  ;$D4
    FCB    $2B  ;$D5
    FCB    $2F  ;$D6
    FCB    $2F  ;$D7
    FCB    $0B  ;$D8
    FCB    $0B  ;$D9
    FCB    $0F  ;$DA
    FCB    $0F  ;$DB
    FCB    $2B  ;$DC
    FCB    $2B  ;$DD
    FCB    $2F  ;$DE
    FCB    $2F  ;$DF
    FCB    $19  ;$E0
    FCB    $19  ;$E1
    FCB    $1D  ;$E2
    FCB    $1D  ;$E3
    FCB    $39  ;$E4
    FCB    $39  ;$E5
    FCB    $3D  ;$E6
    FCB    $3D  ;$E7
    FCB    $19  ;$E8
    FCB    $19  ;$E9
    FCB    $1D  ;$EA
    FCB    $1D  ;$EB
    FCB    $39  ;$EC
    FCB    $39  ;$ED
    FCB    $3D  ;$EE
    FCB    $3D  ;$EF
    FCB    $1B  ;$F0
    FCB    $1B  ;$F1
    FCB    $1F  ;$F2
    FCB    $1F  ;$F3
    FCB    $3B  ;$F4
    FCB    $3B  ;$F5
    FCB    $3F  ;$F6
    FCB    $3F  ;$F7
    FCB    $1B  ;$F8
    FCB    $1B  ;$F9
    FCB    $1F  ;$FA
    FCB    $1F  ;$FB
    FCB    $3B  ;$FC
    FCB    $3B  ;$FD
    FCB    $3F  ;$FE
    FCB    $3F  ;$FF
Joust_Colour_Match:
    FCB    $00  ;$00
    FCB    $00  ;$01
    FCB    $04  ;$02
    FCB    $04  ;$03
    FCB    $20  ;$04
    FCB    $20  ;$05
    FCB    $24  ;$06
    FCB    $24  ;$07
    FCB    $00  ;$08
    FCB    $00  ;$09
    FCB    $04  ;$0A
    FCB    $04  ;$0B
    FCB    $20  ;$0C
    FCB    $20  ;$0D
    FCB    $24  ;$0E
    FCB    $24  ;$0F
    FCB    $02  ;$10
    FCB    $02  ;$11
    FCB    $06  ;$12
    FCB    $06  ;$13
    FCB    $22  ;$14
    FCB    $22  ;$15
    FCB    $26  ;$16
    FCB    $26  ;$17
    FCB    $02  ;$18
    FCB    $02  ;$19
    FCB    $06  ;$1A
    FCB    $06  ;$1B
    FCB    $22  ;$1C
    FCB    $22  ;$1D
    FCB    $26  ;$1E
    FCB    $26  ;$1F
    FCB    $10  ;$20
    FCB    $10  ;$21
    FCB    $14  ;$22
    FCB    $14  ;$23
    FCB    $30  ;$24
    FCB    $30  ;$25
    FCB    $34  ;$26
    FCB    $34  ;$27
    FCB    $10  ;$28
    FCB    $10  ;$29
    FCB    $14  ;$2A
    FCB    $14  ;$2B
    FCB    $30  ;$2C
    FCB    $30  ;$2D
    FCB    $34  ;$2E
    FCB    $34  ;$2F
    FCB    $12  ;$30
    FCB    $12  ;$31
    FCB    $16  ;$32
    FCB    $16  ;$33
    FCB    $32  ;$34
    FCB    $32  ;$35
    FCB    $36  ;$36
    FCB    $36  ;$37
    FCB    $12  ;$38
    FCB    $12  ;$39
    FCB    $16  ;$3A
    FCB    $16  ;$3B
    FCB    $32  ;$3C
    FCB    $32  ;$3D
    FCB    $36  ;$3E
    FCB    $36  ;$3F
    FCB    $01  ;$40
    FCB    $01  ;$41
    FCB    $05  ;$42
    FCB    $05  ;$43
    FCB    $21  ;$44
    FCB    $21  ;$45
    FCB    $25  ;$46
    FCB    $25  ;$47
    FCB    $01  ;$48
    FCB    $01  ;$49
    FCB    $05  ;$4A
    FCB    $05  ;$4B
    FCB    $21  ;$4C
    FCB    $21  ;$4D
    FCB    $25  ;$4E
    FCB    $25  ;$4F
    FCB    $03  ;$50
    FCB    $03  ;$51
    FCB    $07  ;$52
    FCB    $07  ;$53
    FCB    $23  ;$54
    FCB    $23  ;$55
    FCB    $27  ;$56
    FCB    $27  ;$57
    FCB    $03  ;$58
    FCB    $03  ;$59
    FCB    $07  ;$5A
    FCB    $07  ;$5B
    FCB    $23  ;$5C
    FCB    $23  ;$5D
    FCB    $27  ;$5E
    FCB    $27  ;$5F
    FCB    $11  ;$60
    FCB    $11  ;$61
    FCB    $15  ;$62
    FCB    $15  ;$63
    FCB    $31  ;$64
    FCB    $31  ;$65
    FCB    $35  ;$66
    FCB    $35  ;$67
    FCB    $11  ;$68
    FCB    $11  ;$69
    FCB    $15  ;$6A
    FCB    $15  ;$6B
    FCB    $31  ;$6C
    FCB    $31  ;$6D
    FCB    $35  ;$6E
    FCB    $35  ;$6F
    FCB    $13  ;$70
    FCB    $13  ;$71
    FCB    $17  ;$72
    FCB    $17  ;$73
    FCB    $33  ;$74
    FCB    $33  ;$75
    FCB    $37  ;$76
    FCB    $37  ;$77
    FCB    $13  ;$78
    FCB    $13  ;$79
    FCB    $17  ;$7A
    FCB    $17  ;$7B
    FCB    $33  ;$7C
    FCB    $33  ;$7D
    FCB    $37  ;$7E
    FCB    $37  ;$7F

  IF Add_Audio

* Code to handle sound functions when we just completed playing a sample
SampleIsDone:
        LDA   DoSoundLoop    * If DoSoundLoop <> 0 then we loop forever
        BEQ   >              * If DoSoundLoop = 0 then we end sound
        LDA   SampleStart
        STA   GetSample+1
        LDA   SampleStart+1
        STA   GetSample+2   * Sample pointer is now pointing to the start
        BRA   GetSample     * Go play the sample from the beginning again
!
        LDA   #FIRQNoAudio/256
        STA   FIRQ_Start_Address
        LDA   #FIRQNoAudio-(FIRQNoAudio/256*256)
        STA   FIRQ_Start_Address+1      * Change the Sample playback FIRQ to FIRQNoAudio
        CLRA
        BRA     SendAudio

*****************************************
* Audio off - Just re-enable the FIRQ and return
FIRQNoAudio:
        STA     FIRQ0Restore+1    * Save exit value of A
        LDA     FIRQENR           * Re enable the FIRQ
        CLRA
        BRA     SendAudio
        opt     cd
        opt     cc                *
FIRQ_Sound:
        STA     FIRQ0Restore+1    * Save exit value of A
        LDA     FIRQENR           * Re enable the FIRQ
        LDA     #%00100001        *
        STA     INIT1_Register1   * Mem Type 64k chips, 279.365 nsec timer, MMU Task 1 - $FFB0-$FFB7 - Audio sample banks
        INC     GetSample+2       * LSB = LSB + 1
        BNE     >                 * IF we didn't hit zero skip ahead
        INC     GetSample+1       * MSB = MSB + 1
        BMI     SampleIsDone      * If we get to $8000 then we are done playing this sample
  ELSE
        opt     cd
        opt     cc                *
FIRQ_Sound:
        STA     FIRQ0Restore+1    * Save exit value of A
        LDA     FIRQENR           * Re enable the FIRQ
        LDA     #%00100001        *
        STA     INIT1_Register1   * Mem Type 64k chips, 279.365 nsec timer, MMU Task 1 - $FFB0-$FFB7 - Audio sample banks
        INC     SoundBytes+1      * LSB = LSB + 1
        BNE     >                 * IF we didn't hit zero skip ahead
        INC     SoundBytes        * MSB = MSB + 1
*        BMI     SampleIsDone      * If we get to $8000 then we are done playing this sample
  ENDIF
GetSample:
!
        LDA     SoundBytes
SendAudio:
        STA     PIA1_Byte_0_IRQ   * OUTPUT sound to DAC
        LDA     #%00100000        *
        STA     INIT1_Register1   * Mem Type 64k chips, 279.365 nsec timer, MMU Task 0 - $FFA0-$FFA7 - Normal
        DEC     FIRQCount
        BEQ     Do_VideoBeam_IRQ  * We have counted down 25 FIRQs so the Video beam should be down another 1/4 of the screen so go act like an VideoBeam IRQ was triggered for Joust Hardware
FIRQ0Restore:
        LDA     #$FF              * Self modified from above upon FIRQ entry
        RTI                       * Return from the FIRQ
* Act like a Hardware VideoBeam IRQ has been triggered
Do_VideoBeam_IRQ:
        LDA     #FIRQ_Count
        STA     FIRQCount        * 100 FIRQs per frame so each time we count down 25 we will be 1/4 further down the screen (66 rows on the CoCo 3 screen)
        LDA     VideoBeam        * Get the current Videobeam address (either 0,64,128)
        ADDA    #64               * Set the correct value for the videobeam that Joust will need (64,128,196)
        STA     VideoBeam
        BEQ     FIRQ0Restore      * If for some reason the FIRQ triggers just before the VSYNC IRQ then skip it so the VSYNC IRQ will handle it
        LDA     IRQ_Flag          * Check the IRQ flag if = 0 then the IRQ is not being processed, if <> 0 then it is already in progress
        BNE     FIRQ0Restore      * Skip if IRQ is inprogress as we don't want to coninously do the IRQ
;        COM     IRQ_Flag         * Set the IRQ flag as active
        LDA     ,S+               * A= the CC off the stack fix the stack pointer
        PSHS    B,DP,X,Y,U        * Save the registers as the normal IRQ would have done (A was already saved - self modified code)
        LDB     FIRQ0Restore+1    * Restore entry value of A
        STB     ,-S               * Save A on the Stack (really it's A not B)
        LDX     #FIRQ_ReturnsHere * return address from the FIRQ RTI
        PSHS    A,X               * This really saves the CC and the return address below so the RTI will restore the CC and return below
        ANDCC   #%10111111        * Clear the FIRQ flag so the FIRQ can happen while doing the Joust IRQ

* From Joust IRQ
	INC	RANDOM
	DEC	RANDOM+1
	LDA	DRRUC		DMA READS ROM!
	ORA	#1
	STA	RRUC

        JMP     IRQ2          * Joust_IRQ_Triggered
FIRQ_ReturnsHere:
        PULS    A,B,DP,X,Y,U,PC

*****************************************
VideoBeam  					FCB $0      * Where the current scanline is being drawn on screen
FIRQ_Countdown      EQU $0254  	* $254=6005.950455 samples per second
FIRQ_Between_IRQs   EQU 99   	  * Tested with MAME, 99 times the FIRQ was triggered between each VSYNC IRQ
IRQ_Delay_Count     EQU 59   	  * MAME scanline delay count before the scanline should be in the middle of the screen (From CoCo 3 Mame info...)
SoundBytes          FDB 0
DoSoundLoop         FCB 0       * 0 = No loop, <> 0 = Loop sound
SampleStart         FDB $7FFF
KeyboardOrJoystick  FCB 0       * 0 = Keyboard, <> = Joystick
CMOS_P1FColumn      EQU $D004   * My Fake CMOS storage for user selected keycodes
FIRQCount           FCB 0       * FIRQ counts this value down to keep track of where the scanline is being drawn
IRQ_Flag            FCB 0       * 0 if IRQ is not in progress, 1 is processing the IRQ

RRUC                FCB 1       * real Joust hardware would force lower memory from/to RAM mode or ROM mode, probably best to remove any use of
                                * it but for now it's here to match Joust, original would write to memery location $C900 which we don't want

PIA2        RMB 1   * Temp storage of B for the section to enter your initals  $C804
WPIAA_P1   FCB $00  * Bits 5=1 player start,4=2 player start,3=NotUsed,2=P1_Flap,1=P1_JoyRight,0=P1_JoyLeft  $C804 - A INPUT		WIDGET-BRD INPUTS;
WPIAA_P2   FCB $00  * Bits 2=P2_Flap, 1=P2_JoyRight, 0=P2_JoyLeft;
WCPIAA  RMB $01     * I/O borad PIA CTRL_A  $C805 - A CONTROL	GAME CNTRLS, PLYR1 AND 2 STARTS
WPIAB   RMB $01     *                              - B INPUT
WCPIAB  RMB $01     * I/O borad PIA CTRL_B  $C807 - B CONTROL
PIAA    RMB $01     * Bits7=HighScore_Reset,6=Tilt,5=CoinSlot2,4=CoinSlot1,3=NotUsed,2=CoinSlot3,1=Advance_Button,0=AutoUp/Man Down $C80C - COIN DOOR,INTERRUPT,SOUND BRD PIA'S
CPIAA   RMB $01     * PIA Control A setting  $C80D - 16MS CONTROL - CA1
PIAB    RMB $01     * PIA Data B reading/writing $C80E PIA Data B reading/writing - bit 0 to 5 = 6 bits to sound board, bits 6 & 7 plus CA2 and CB2 = 4 bits to drive the LED 7 segment - SOUND BOARD OUTPUTS
CPIAB   RMB $01     * PIA Control B setting $C80F - 4MS CONTROL - CB1	B0=1 ENABLE  B0=0 DISABLE

Blitter_Execute     RMB 1     * Value sets the blitter type to do
Blitter_Mask        RMB 1     * Basically the palette of the set bits
Blitter_SourceAddress RMB 2   * Address in RAM where the sprite exists
Blitter_DestinationAddress    RMB   2  * Place in Video memory to show the sprite
Blitter_Width       RMB 1     * Width of the sprite
Blitter_Height      RMB 1     * Height of the sprite

BlitRows    RMB 1
BoxLocation RMB 2     * Value set when drawing first attract screen (High scores)

BlockHeight RMB 1     * Temp storage to keep track of height of block/sprite to copy

RAM_ROM_Mode_0_is_RAM RMB 1   * Set ROM or RAM mode 1=ROM mode, 0=RAM mode

********************************************************
* This section shoud be in RAM at $B208 to $BBFF anything lower and it will be erased and written over when game starts up
* useable space is from $B208 to $BBFF

		ORG		$B208

* Clear screen just as the Blitter would
;	LDD	#$9CD7    * XOR with $0404 gives us $98D3 = $98 = 152 wide = $D3 = 211 height, We will clear to Row 211 = 160*211 = 33,760 = $83E0
ClearScreenLikeBlitter:
* Set lower RAM to screen mode
		LDD			#$0001
    STD     MMU_Reg_Bank0_0  	* Set Banks 0 & 1 - Graphics RAM banks
    LDD     #$0203
    STD     MMU_Reg_Bank0_2  	* Set Banks 2 & 3 - Graphics RAM banks
    LDA			#$04
    STA     MMU_Reg_Bank0_4  	* Set Bank 4 		 - Graphics RAM bank
	PSHS	CC
	ORCC	#$FF    * Disable the Interrupts
  PSHS  DP,Y,U  * Save some registers
  STS   ClearScreenLikeBlitter_Stack+2    * Save the stack (self modify below)
  LDS   #$83E0  * Bottom of the screen
  LDD   #$0000
  TFR   A,DP
  LDX   #$0000
  LEAY  ,X
  LDU   #$0000
!
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U * 36
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U * 72
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U * 108
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U
  PSHS  D,DP,X,Y,U * 144
  PSHS  D,DP,X,Y,U * 153
  PSHS  D,DP,X,Y   * 160   Blast fast as we turned off audio
  CMPS  #$0000
  BNE   <
ClearScreenLikeBlitter_Stack:
  LDS   #$0000  * Restore the Stack (self modified from above)
* Put blocks back to normal
    LDD     #$3839
    STD     MMU_Reg_Bank0_0  * Set Banks 0 & 1
    LDD     #$3A3B
    STD     MMU_Reg_Bank0_2  * Set Banks 2 & 3
    LDA			#$3C             * A=$3C
    STA     MMU_Reg_Bank0_4  * Set Bank 4
  PULS  DP,Y,U  * Restore the registers
	PULS	CC,PC   * Restore the CC to what it was and Return

* Blast a rectangle on screen
* A = Block description:  Usually a $1A or $12
* B = Colour of the block
* X = Source address and can be ignored
* Y = Destination address
* U points to the width & height of the block (Exclusive OR'd with $04, for the width and then again for the Height)
* This will be changed to the jump address below
; A has this info
;DMA BITS: H,L,F,K Z,S,W,R
;                        R-READ BLOCK
;                      W-WRITE BLOCK
;                    S-SLOW FOR RAM
;                  Z-SUPRESS ZEROES
;                K-REPLACE DATA WITH CONSTANT
;              F-FLAVOR RIGHT
;            L-SUPRESS CONSTANT LOW NIBBLE
;          H-SUPRESS CONSTANT HIGH NIBBLE
;=12 =   K,W A=88 -
;=1A = K,Z,W A=00 - It cleared some memory, if A has a value then it used if the destination is not zero
;=0A =   Z,W A=00 - 2C6C,64BF - Copied sprite to screen any zeros in the source are transparent
;=2A =   Z,W A=00 - 2C6C,64BF - Copied sprite to screen shifted one pixel to the right any zeros in the source are transparent
BlastRectangle:
    TFR   B,A     * A = colour of the rectangle
	  LDB   #BlastRectangleCode        * Code to Blast the rectangle on screen
    STB   MMU_Reg_Bank0_6            * Set Bank 6
    LDX   #BlastRectangleJumpList    * X points to the start of the table
    LDB   ,U+     * B = width
    EORB  #$04    * B = real width now  (3 to 15)
    CMPB  #15
    LBHI  DrawHugeBlockGotWidthInB   * If the width is too wide go draw a block the slow way
    SUBB  #3      * Make the width 0 to 12
    STB   BlastRectangle1+1 * Save B so we can add it below, self modify code
    LSLB          * B = B * 2
    LSLB          * B = B * 4
    LSLB          * B = B * 8
BlastRectangle1:
    ADDB  #$FF    * B = B + original B, therefore B = B * 9
    ABX           * X=X+B*9 = B=B*9
    ABX           * X=X+B*9 = B=B*18
    ABX           * X=X+B*9 = B=B*27
    ABX           * X=X+B*9 = B=B*36 (18 x 2 byte width entries per Height)
* X now has the correct table entry to get the jump location from
    LDB   ,U+     * B = height
    EORB  #$04    * B = real height now (5 to 20)
    CMPB  #21
    LBHI  DrawHugeBlockGotHeightInB   * If the height is too tall go draw a block the slow way
    SUBB  #3      * Make the height 0 to 17
    BMI   SmallRectangle    * If it's less then a height of 3 then blast it from another list
    LSLB          * B = B * 2 (two bytes per entry in the table)
    ABX           * X now has the correct table entry to jump from
    PSHS  DP,U    * Save the DP as we are going to use it for stack blasting & also save U
    TFR   A,DP    * DP now has the proper colour of hte rectangle
    TFR   A,B     * D now has the proper colour for MSB and LSB
    TFR   D,U     * U now has the proper colour of the rectangle (save this value for later)
    TFR   Y,D     * D now has the Joust screen location
  CMPA    #EndofScreen  * Check if the X co-ordiante is higher then 160 (160 bytes across the screen)
    BLO     >     * If lower then ok to draw, skip ahead
  IF TestScreenEdge
    LDA     #EndofScreenNew
    DECB
  ELSE
    PULS  DP,U,PC * Retore the DP and U and return
  ENDIF
!
    STA   BlastRectangle2+2   * Save A in selfmod ADDD below
    LDA   #160                * D = B * 160 (Y co-ordinate * 160)
    MUL
BlastRectangle2:
    ADDD  #$0000              * Self modified from above
    EXG   D,U                 * D now has the proper colour of the rectangle and U points to the start of the block for the CoCo3 screen location
    TFR   D,Y                 * Y also has the colour of the rectangle
    LDX   ,X      * X now points to the code that will draw the rectangle
    CMPX  #$E000  * Is the jump going to the next bank?
    BLO   >       * If not skip ahead, otherwise X=X-$2000 and make Bank 6 BlastRectangleCode1
    LEAX  -$2000,X * X now points to Bank 6
	  LDB   #BlastRectangleCode1       * Code to Blast the rectangle on screen (2nd block)
    STB   MMU_Reg_Bank0_6            * Set Bank 6
    TFR   A,B       * Restore B with the correct colour
!
    JMP   ,X

* Make a giant jump table sizes of sprites width from 3 to 15 and width from 3 to 20
* make width  0 to 12  we -3
* make height 0 to 17  we -3

* Do smaller blasts as a separate list
SmallRectangle:
    LDX   #SmallBlastRectangleJumpList    * X points to the start of the small jump list
    ADDB  #2      * Fix the height value B is now either 0 or a 1
    LSLB          * B=B*2 it's now either a 0 or a 2  (2 bytes per table entry)
    ABX           * X points to the correct Height
    LDB   -2,U    * B = width
    EORB  #$04    * B = real width now  (0 to 15)
    LSLB          * B = B * 2
    LSLB          * B = B * 4 (4 bytes per width entries)
    ABX           * X=X+B*8 (16 x 2 byte width entries per Height)
	  LDB   #BlastRectangleCode        * Code to Blast the rectangle on screen
    STB   MMU_Reg_Bank0_6            * Set Bank 6
    PSHS  DP,U    * Save the DP as we are going to use it for stack blasting & also save U
    TFR   A,DP    * DP now has the proper colour of the rectangle
    TFR   A,B     * D now has the proper colour for MSB and LSB
    TFR   D,U     * U now has the proper colour of the rectangle (save this value for later)
    TFR   Y,D     * D now has the Joust screen location
  CMPA    #EndofScreen  * Check if the X co-ordiante is higher then 160 (160 bytes across the screen)
    BLO     >     * If lower then ok to draw, skip ahead
  IF TestScreenEdge
    LDA     #EndofScreenNew
    DECB
  ELSE
    PULS  DP,U,PC * Retore the DP and U and return
  ENDIF
!
    STA   BlastRectangle3+2   * Save A in selfmod ADDD below
    LDA   #160                * D = B * 160 (Y co-ordinate * 160)
    MUL
BlastRectangle3:
    ADDD  #$0000              * Self modified from above
    EXG   D,U                 * D now has the proper colour of the rectangle and U points to the start of the block for the CoCo3 screen location
    TFR   D,Y                 * Y also has the colour of the rectangle
    JMP   [,X]
*
* Do a Big block copy of pattern A a lot slower then above stack blasting version but can handle very large sizes
*
* U needs to read the Height and move forward so it will be ready for the next sprite info
DrawHugeBlockGotWidthInB:
  LEAU    1,U                 * Move the U pointer forward 1
* U has been moved forward and ready for next sprite info
DrawHugeBlockGotHeightInB:
* U is now where it needs to be
  STA     WriteBigBlockB+1    * Save A = the Colour of the block - self modify
  TFR     Y,D                 * D = Y = Destination Start address
  CMPA    #EndofScreen  * Check if the X co-ordiante is higher then 160 (160 bytes across the screen)
  BLO     >                   * If lower then ok to draw, skip ahead
  IF TestScreenEdge
    LDA     #EndofScreenNew
    DECB
  ELSE
    LEAU    2,U         * Otherwise don't draw it, move U to the next entry
    BRA     DoneWritingBlock    * skip over writing sprite code
  ENDIF
!
  STA     WriteBigBlock+2     * Save A in selfmod ADDD below
  LDA     #160                * D = B * 160 (Y co-ordinate * 160)
  MUL
WriteBigBlock:
  ADDD    #$0000              * Self modified from above
  TFR     D,X                 * X now has proper CoCo3 screen location
  LDD     -2,U                * A = Width and B = the height of the block to copy, U already points to the next sprite location
  EORA    #$04                * Fix Width
  STA     BlockNextRow+1      * Save it for later
  NEGA
  ADDA    #160                * A = -Width+160 = start of next row
  STA     BlockNextRow+2      * Save it for later
  LDA     BlockNextRow+1      * Get width again
  EORB    #$04                * Fix Height
  STB     BlockHeight         * Save the height
  BRA     WriteBigBlockB      * Skip over moving X for the first row
* Draw block A wide & B High
BlockNextRow:
  LDD     #$0000              * Self modified from above A = width, B=width of a row =160 bytes - width of block to create
  ABX                         * Move X to the start of the next row
WriteBigBlockB:
  LDB     #$00                * Self modified value from above
!
  STB     ,X+
  DECA
  BNE     <
  DEC     BlockHeight
  BNE     BlockNextRow
DoneWritingBlock:
  RTS


	IF UseCompiledSprites
* 0000   WCDMA   RMB     1       WRITE/CLEAR'S DMA CONTROL BYTE
* 0001   WCCON   RMB     1       WRITE/CLEAR'S DMA CONSTANT BYTE
* 0002   WCSRC   RMB     2       WRITE/CLEAR'S DMA SOURCE ADDRESS
* 0004   WCDEST  RMB     0       WRITE/CLEAR'S DMA DESTINATION ADDRESS
* 0004   WCX     RMB     1       WRITE/CLEAR'S DMA DESTINATION X PIXEL LOCATION
* 0005   WCY     RMB     1       WRITE/CLEAR'S DMA DESTINATION Y PIXEL LOCATION
* 0006   WCLEN   RMB     0       WRITE/CLEAR'S DMA X,Y LENGTH (NO INVERSION)
* 0006   WCLENX  RMB     1       WRITE/CLEAR'S DMA X LENGTH (NO INVERSION)
* 0007   WCLENY  RMB     1       WRITE/CLEAR'S DMA Y LENGTH (NO INVERSION)

MyCLIPIT:
  LDB     WCDMA,X
  BITB    #%00010000     * Is it a block copy to do? = $1A or $12?
  BNE     LeaveBlockAsIs * if bit 4 is a 1 then it's a block copy so handle it like normal

	NEGA              * A = WCY Destination address   A=DF=11011111, becomes 21=00100001
	ADDA	  #FLOOR+1	* NEW LENGTH  FLOOR+1 = #$E0 (Y value or height of the LAVA on screen)
                    * A now has the heght of the sprite from top to where the lava is
                    * we need to calc the length of the original height - A so rectangle is height - A
                    * the destination Y is the LAVA height is FLOOR+1 = #$E0 (Y value or height of the LAVA on screen)
  STA     MyCLIPIT1+1  * Self mod subtraction byte in the next line so B=B-A

  LDB     WCLENY,X  * Get the original sprite height in B
	EORB	  #$04				;!WDMAFIX
MyCLIPIT1:
  SUBB    #$FF      * B now has the correct height of the lava rectangle to draw over the bottom of the sprite
  BEQ     >         * If the new size is zero then skip clipping
	EORB	  #$04				;!WDMAFIX
	STB	    WCLENY,X  * Save the new value of the height of the lava rectangle
  LDB     #FLOOR+1  * B = destination Y value of the Lava
  STB     WCCON,X   * save it as the value to be used to start drawing the rectangle (special sprite drawing code will use this)

  LDB     WCDMA,X
  ORB     #%10000000     * Set bit 7 which indicates it's a special sprite with lava rectangle to draw
  STB     WCDMA,X        * Save it
!
	RTS
LeaveBlockAsIs:
	NEGA              * A = WCY Destination address   A=DF=11011111, becomes 21=00100001
	ADDA	  #FLOOR+1	* NEW LENGTH  FLOOR+1 = #$E0 (Y value or height of the LAVA on screen)
	EORA	#$04				;!WDMAFIX
	STA	WCLENY,X
	RTS
  ENDIF

	IF Add_Audio
* Sound playback table
SampleBlockTable:
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 00 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 01 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 02 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 03 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 04 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 05 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 06 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 07 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 08 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 09 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 0A - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 0B - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 0C - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 0D - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 0E - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 0F - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 10 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 11 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 12 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 13 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 14 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 15 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 16 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 17 - Silence
  FCB BLK_58_End_Skidding,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_58_Start       *  Start address of sample playback 18 - 58_End_Skidding
  FCB BLK_59_Skidding_Sound,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_59_Start       *  Start address of sample playback 19 - 59_Skidding_Sound
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 1A - Silence
  FCB Cliff5_And_Scoring,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_5B_Start       *  Start address of sample playback 1B - 5B = sound DB_PTERODACTYL_INTRODUCTION_SCREAM
  FCB FontPalette00,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_5C_Start       *  Start address of sample playback 1C - 5C_Enemy_Runs
  FCB FontPalette01,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_5D_Start       *  Start address of sample playback 1D - 5D_Enemy_Runs2
  FCB BLK_5E_Enemy_Wing_Up,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_5E_Start       *  Start address of sample playback 1E - 5E_Enemy_Wing_Up
  FCB BLK_5F_Enemies_Wing_Down,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_5F_Start       *  Start address of sample playback 1F - 5F_Enemies_Wing_Down
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 20 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 21 - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 22 - Silence
  FCB BLK_63_Collect_Bounty_Short,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_63_Start       *  Start address of sample playback 23 - 63_Collect_Bounty_Short
  FCB BLK_64_Start_Game_Short,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_64_Start       *  Start address of sample playback 24 - 64_Start_Game_Short
  FCB BLK_E5_Part_Of_High_Score,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_E5_Start       *  Start address of sample playback 25 - E5_Part_Of_High_Score
  FCB BLK_66_Cliff_Destroyer_Short,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_66_Start       *  Start address of sample playback 26 - 66_Cliff_Destroyer_Short
  FCB BLK_E7_Part_Of_High_Score,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_E7_Start       *  Start address of sample playback 27 - E7_Part_Of_High_Score
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 28 - Silence
  FCB BLK_E9_PTERODACTYL_DYING_SOUND,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_69_Start       *  Start address of sample playback 29 - 69 = E9_PTERODACTYL_DYING_SOUND
  FCB BLK_EA_Player_Fading_In_Transporter,$01  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_EA_Start       *  Start address of sample playback 2A - EA_Player_Fading_In_Transporter
  FCB BLK_EB_Player_Fading_In_Transporter2,$01  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_EB_Start       *  Start address of sample playback 2B - EB_Player_Fading_In_Transporter2
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 2C - Silence
  FCB BLK_ED_Player_Created_In_Transporter2,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_ED_Start       *  Start address of sample playback 2D - ED_Player_Created_In_Transporter2
  FCB BLK_EE_Part_Of_High_Score,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_EE_Start       *  Start address of sample playback 2E - EE_Part_Of_High_Score
  FCB BLK_EF_Part_Of_High_Score,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_EF_Start       *  Start address of sample playback 2F - EF_Part_Of_High_Score
  FCB BLK_F0_Part_Of_High_Score,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_70_Start       *  Start address of sample playback 30 - 70 = F0_Part_Of_High_Score
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 31 - Silence
  FCB CMOS_Storage,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_72_Start       *  Start address of sample playback 32 - 72_Enemy_or_Player_In_Lava
  FCB BLK_73_Enemy_Mounting_Bird,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_73_Start       *  Start address of sample playback 33 - 73_Enemy_Mounting_Bird
  FCB BLK_74_Extra_Man,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_74_Start       *  Start address of sample playback 34 - 74_Extra_Man
  FCB BLK_75_Credit_Inserted,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_75_Start       *  Start address of sample playback 35 - 75_Credit_Inserted
  FCB BLK_76_CAPTURED_BY_LAVA_TROLL,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_76_Start       *  Start address of sample playback 36 - 76_CAPTURED_BY_LAVA_TROLL
  FCB BLK_F7_Enemy_Thud,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_F7_Start       *  Start address of sample playback 37 - F7_Enemy_Thud
  FCB BLK_F8_ENEMY_RECREATED,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_F8_Start       *  Start address of sample playback 38 - F8_ENEMY_RECREATED
  FCB BLK_79_Cliff_Thud,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_79_Start       *  Start address of sample playback 39 - 79_Cliff_Thud
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 3A - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 3B - Silence
  FCB BLK_7C_Player_Hits_Egg,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_7C_Start       *  Start address of sample playback 3C - 7C_Player_Hits_Egg
  FCB BLK_7D_Egg_Hatching,$00  *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB Sample_7D_Start       *  Start address of sample playback 3D - 7D_Egg_Hatching
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 3E - Silence
  FCB $37,$00               *  Block to use for sound, Loop indicator 0 = no loop, <> 0 = Yes loop
  FDB $7FFF                 *  Start address of sample playback 3F - Silence
  ENDIF

	IF UseCompiledSprites
TransporterSpriteTable:
* Enemies
EnemyPlyTransRight:
    FDB     PLYxRCS_White1
    FDB     PLYxRCS_White2
    FDB     PLYxRCS_White3
    FDB     PLYxRCS_White4
    FDB     PLYxRCS_White5
    FDB     PLYxRCS_White6
    FDB     PLYxRCS_White7
EnemyPlyTransLeft:
    FDB     PLYxLCS_White1
    FDB     PLYxLCS_White2
    FDB     PLYxLCS_White3
    FDB     PLYxLCS_White4
    FDB     PLYxLCS_White5
    FDB     PLYxLCS_White6
    FDB     PLYxLCS_White7
EnemyBuzTransRight:
    FDB     BRUN4RCS_White1
    FDB     BRUN4RCS_White2
    FDB     BRUN4RCS_White3
    FDB     BRUN4RCS_White4
    FDB     BRUN4RCS_White5
    FDB     BRUN4RCS_White6
    FDB     BRUN4RCS_White7
    FDB     BRUN4RCS_White8
    FDB     BRUN4RCS_White9
    FDB     BRUN4RCS_White10
    FDB     BRUN4RCS_White11
    FDB     BRUN4RCS_White12
    FDB     BRUN4RCS_White13
    FDB     BRUN4RCS_White14
EnemyBuzTransLeft:
    FDB     BRUN4LCS_White1
    FDB     BRUN4LCS_White2
    FDB     BRUN4LCS_White3
    FDB     BRUN4LCS_White4
    FDB     BRUN4LCS_White5
    FDB     BRUN4LCS_White6
    FDB     BRUN4LCS_White7
    FDB     BRUN4LCS_White8
    FDB     BRUN4LCS_White9
    FDB     BRUN4LCS_White10
    FDB     BRUN4LCS_White11
    FDB     BRUN4LCS_White12
    FDB     BRUN4LCS_White13
    FDB     BRUN4LCS_White14

* Player 1 Yellow - $55
Ply1TransRWhite:
    FDB     PLY1RCS_White1
    FDB     PLY1RCS_White2
    FDB     PLY1RCS_White3
    FDB     PLY1RCS_White4
    FDB     PLY1RCS_White5
    FDB     PLY1RCS_White6
    FDB     PLY1RCS_White7
Ply1TransRYellow:
    FDB     PLY1RCS_Yellow1
    FDB     PLY1RCS_Yellow2
    FDB     PLY1RCS_Yellow3
    FDB     PLY1RCS_Yellow4
    FDB     PLY1RCS_Yellow5
    FDB     PLY1RCS_Yellow6
    FDB     PLY1RCS_Yellow7
Ply1TransLWhite:
    FDB     PLY1LCS_White1
    FDB     PLY1LCS_White2
    FDB     PLY1LCS_White3
    FDB     PLY1LCS_White4
    FDB     PLY1LCS_White5
    FDB     PLY1LCS_White6
    FDB     PLY1LCS_White7
Ply1TransLYellow:
    FDB     PLY1LCS_Yellow1
    FDB     PLY1LCS_Yellow2
    FDB     PLY1LCS_Yellow3
    FDB     PLY1LCS_Yellow4
    FDB     PLY1LCS_Yellow5
    FDB     PLY1LCS_Yellow6
    FDB     PLY1LCS_Yellow7
OTransRWhite:
    FDB     ORUN4RCS_White1
    FDB     ORUN4RCS_White2
    FDB     ORUN4RCS_White3
    FDB     ORUN4RCS_White4
    FDB     ORUN4RCS_White5
    FDB     ORUN4RCS_White6
    FDB     ORUN4RCS_White7
    FDB     ORUN4RCS_White8
    FDB     ORUN4RCS_White9
    FDB     ORUN4RCS_White10
    FDB     ORUN4RCS_White11
    FDB     ORUN4RCS_White12
    FDB     ORUN4RCS_White13
    FDB     ORUN4RCS_White14
    FDB     ORUN4RCS_White15
    FDB     ORUN4RCS_White16
    FDB     ORUN4RCS_White17
    FDB     ORUN4RCS_White18
    FDB     ORUN4RCS_White19
    FDB     ORUN4RCS_White20
OTransRYellow:
    FDB     ORUN4RCS_Yellow1
    FDB     ORUN4RCS_Yellow2
    FDB     ORUN4RCS_Yellow3
    FDB     ORUN4RCS_Yellow4
    FDB     ORUN4RCS_Yellow5
    FDB     ORUN4RCS_Yellow6
    FDB     ORUN4RCS_Yellow7
    FDB     ORUN4RCS_Yellow8
    FDB     ORUN4RCS_Yellow9
    FDB     ORUN4RCS_Yellow10
    FDB     ORUN4RCS_Yellow11
    FDB     ORUN4RCS_Yellow12
    FDB     ORUN4RCS_Yellow13
    FDB     ORUN4RCS_Yellow14
    FDB     ORUN4RCS_Yellow15
    FDB     ORUN4RCS_Yellow16
    FDB     ORUN4RCS_Yellow17
    FDB     ORUN4RCS_Yellow18
    FDB     ORUN4RCS_Yellow19
    FDB     ORUN4RCS_Yellow20
OTransLWhite:
    FDB     ORUN4LCS_White1
    FDB     ORUN4LCS_White2
    FDB     ORUN4LCS_White3
    FDB     ORUN4LCS_White4
    FDB     ORUN4LCS_White5
    FDB     ORUN4LCS_White6
    FDB     ORUN4LCS_White7
    FDB     ORUN4LCS_White8
    FDB     ORUN4LCS_White9
    FDB     ORUN4LCS_White10
    FDB     ORUN4LCS_White11
    FDB     ORUN4LCS_White12
    FDB     ORUN4LCS_White13
    FDB     ORUN4LCS_White14
    FDB     ORUN4LCS_White15
    FDB     ORUN4LCS_White16
    FDB     ORUN4LCS_White17
    FDB     ORUN4LCS_White18
    FDB     ORUN4LCS_White19
    FDB     ORUN4LCS_White20
OTransLYellow:
    FDB     ORUN4LCS_Yellow1
    FDB     ORUN4LCS_Yellow2
    FDB     ORUN4LCS_Yellow3
    FDB     ORUN4LCS_Yellow4
    FDB     ORUN4LCS_Yellow5
    FDB     ORUN4LCS_Yellow6
    FDB     ORUN4LCS_Yellow7
    FDB     ORUN4LCS_Yellow8
    FDB     ORUN4LCS_Yellow9
    FDB     ORUN4LCS_Yellow10
    FDB     ORUN4LCS_Yellow11
    FDB     ORUN4LCS_Yellow12
    FDB     ORUN4LCS_Yellow13
    FDB     ORUN4LCS_Yellow14
    FDB     ORUN4LCS_Yellow15
    FDB     ORUN4LCS_Yellow16
    FDB     ORUN4LCS_Yellow17
    FDB     ORUN4LCS_Yellow18
    FDB     ORUN4LCS_Yellow19
    FDB     ORUN4LCS_Yellow20

* Player 2 - Blue - $77
Ply2TransRWhite:
    FDB     PLY2RCS_White1
    FDB     PLY2RCS_White2
    FDB     PLY2RCS_White3
    FDB     PLY2RCS_White4
    FDB     PLY2RCS_White5
    FDB     PLY2RCS_White6
    FDB     PLY2RCS_White7
Ply2TransRBlue:
    FDB     PLY2RCS_Blue1
    FDB     PLY2RCS_Blue2
    FDB     PLY2RCS_Blue3
    FDB     PLY2RCS_Blue4
    FDB     PLY2RCS_Blue5
    FDB     PLY2RCS_Blue6
    FDB     PLY2RCS_Blue7
Ply2TransLWhite:
    FDB     PLY2LCS_White1
    FDB     PLY2LCS_White2
    FDB     PLY2LCS_White3
    FDB     PLY2LCS_White4
    FDB     PLY2LCS_White5
    FDB     PLY2LCS_White6
    FDB     PLY2LCS_White7
Ply2TransLBlue:
    FDB     PLY2LCS_Blue1
    FDB     PLY2LCS_Blue2
    FDB     PLY2LCS_Blue3
    FDB     PLY2LCS_Blue4
    FDB     PLY2LCS_Blue5
    FDB     PLY2LCS_Blue6
    FDB     PLY2LCS_Blue7
STransRWhite:
    FDB     SRUN4RCS_White1
    FDB     SRUN4RCS_White2
    FDB     SRUN4RCS_White3
    FDB     SRUN4RCS_White4
    FDB     SRUN4RCS_White5
    FDB     SRUN4RCS_White6
    FDB     SRUN4RCS_White7
    FDB     SRUN4RCS_White8
    FDB     SRUN4RCS_White9
    FDB     SRUN4RCS_White10
    FDB     SRUN4RCS_White11
    FDB     SRUN4RCS_White12
    FDB     SRUN4RCS_White13
    FDB     SRUN4RCS_White14
    FDB     SRUN4RCS_White15
    FDB     SRUN4RCS_White16
    FDB     SRUN4RCS_White17
    FDB     SRUN4RCS_White18
    FDB     SRUN4RCS_White19
    FDB     SRUN4RCS_White20
STransRBlue:
    FDB     SRUN4RCS_Blue1
    FDB     SRUN4RCS_Blue2
    FDB     SRUN4RCS_Blue3
    FDB     SRUN4RCS_Blue4
    FDB     SRUN4RCS_Blue5
    FDB     SRUN4RCS_Blue6
    FDB     SRUN4RCS_Blue7
    FDB     SRUN4RCS_Blue8
    FDB     SRUN4RCS_Blue9
    FDB     SRUN4RCS_Blue10
    FDB     SRUN4RCS_Blue11
    FDB     SRUN4RCS_Blue12
    FDB     SRUN4RCS_Blue13
    FDB     SRUN4RCS_Blue14
    FDB     SRUN4RCS_Blue15
    FDB     SRUN4RCS_Blue16
    FDB     SRUN4RCS_Blue17
    FDB     SRUN4RCS_Blue18
    FDB     SRUN4RCS_Blue19
    FDB     SRUN4RCS_Blue20
STransLWhite:
    FDB     SRUN4LCS_White1
    FDB     SRUN4LCS_White2
    FDB     SRUN4LCS_White3
    FDB     SRUN4LCS_White4
    FDB     SRUN4LCS_White5
    FDB     SRUN4LCS_White6
    FDB     SRUN4LCS_White7
    FDB     SRUN4LCS_White8
    FDB     SRUN4LCS_White9
    FDB     SRUN4LCS_White10
    FDB     SRUN4LCS_White11
    FDB     SRUN4LCS_White12
    FDB     SRUN4LCS_White13
    FDB     SRUN4LCS_White14
    FDB     SRUN4LCS_White15
    FDB     SRUN4LCS_White16
    FDB     SRUN4LCS_White17
    FDB     SRUN4LCS_White18
    FDB     SRUN4LCS_White19
    FDB     SRUN4LCS_White20
STransLBlue:
    FDB     SRUN4LCS_Blue1
    FDB     SRUN4LCS_Blue2
    FDB     SRUN4LCS_Blue3
    FDB     SRUN4LCS_Blue4
    FDB     SRUN4LCS_Blue5
    FDB     SRUN4LCS_Blue6
    FDB     SRUN4LCS_Blue7
    FDB     SRUN4LCS_Blue8
    FDB     SRUN4LCS_Blue9
    FDB     SRUN4LCS_Blue10
    FDB     SRUN4LCS_Blue11
    FDB     SRUN4LCS_Blue12
    FDB     SRUN4LCS_Blue13
    FDB     SRUN4LCS_Blue14
    FDB     SRUN4LCS_Blue15
    FDB     SRUN4LCS_Blue16
    FDB     SRUN4LCS_Blue17
    FDB     SRUN4LCS_Blue18
    FDB     SRUN4LCS_Blue19
    FDB     SRUN4LCS_Blue20

* Write the sprites that are coming out of a transporter (bit 6 of A is set high to indicate it)
* B = the Colour of the sprite - Enemies are always white
* X = the Source data location:
* 3,X = sprite table start location 5,X is the MMU block, 6,X is the Amount to add to get a colour entry in the table
* Y = the Joust screen destination
* LDD ,U++  A & B Xor #$04 A = width of block, B=Height - Use B for table entry that X is pointing to
WriteSprite_Trans:
  CMPA  #%01001111      * Use this as a flag to indicate it's a coloured Transporter to draw
  BNE   >               * If it's not a transporter then skip ahead
* We already setup the sprite info for the colour transformers so we can just use that info to speed up drawing
* B = MMU block, X points to address of compiled sprite code to draw, Y = CoCo 3 address to draw sprite
  STB   MMU_Reg_Bank0_6 * Set Bank 6 - so we can get the address of the sprite drawing routine
  JSR   ,X
  LEAU  2,U             * Fix the pointer
  LBRA  DoneWritingSprite
!
  LDA     #$38                * Bank 0
  CMPX  #$2000                * Is the pointer less then the first block?
  BLO   >
  INCA                        * Bank 1
  LEAX  -$2000,X              * Reduce the pointer by $2000
!
  STA   MMU_Reg_Bank0_6     * Set Bank 6 - so we can get the address of the sprite drawing routine
  LEAX  $C000,X             * X = X +$C000 so it points to the proper location
  LDD   5,X     * A = MMU block for this sprite code, B is the addition for Colour
  LDX   3,X     * Sprite table for this Sprite
  STA   MMU_Reg_Bank0_6
  STB   WriteSprite_Trans2+1  * Save it in selfmod code below
  LDD   ,U++
  EORB  #$04    * B now has the height of this sprite
  LSLB          * B=B*2 (Table entries are 2 bytes each)
  ABX           * X now points to the table entry that points to the Sprite code
  TFR   Y,D     * D=Y = Joust hardware screen location
  STA   WriteSprite_Trans1+2      * Save A in selfmod ADDD below
  LDA   #160    * D = B * 160 (Y co-ordinate * 160)
  MUL
WriteSprite_Trans1:
  ADDD    #$0000  * Self modified from above
  TFR     D,Y     * Y now has proper CoCo3 screen location
  LDB     -7,U  * Get colour value again
  ANDB    #$0F  * ignore the High nibble
  DECB          * White now is 0, Yellow for Player 1 is now 4, Blue for Player 2 is now 6
  BEQ     >     * If it was 1 and is now 0 then it's white so just jump to the table entry X is pointing to
* Deal with Coloured sprites $
WriteSprite_Trans2:
  LDB     #$FF   * Self modified from above
  ABX            * add to X the amount we need to move in the sprite code table to draw a coloured version
!
  JSR   [,X]      * Go draw the Transporter sprite
  LBRA  DoneWritingSprite
HandleTransSprites:
  PSHS  B          * B has the colour of this sprite, save it for later
  LDD   #TRASRC    * is the source address of the Transporter
  CMPD  WCSRC,X    * is it the same as the source address of this sprite?
  BEQ   ColourTransformer  * if yes, go handle Transporter colour
	LDA	  WCDMA,X
  ORA   #%01000000 * Flag it as a special transporter sprite to draw
  PULS  B          * Get the colour to be used for this sprite
L049_001A:
	STD	  WCDMA,X
  RTS
ColourTransformer:
  LDD     4,X              * get the Joust screen location - we might as well convert the address here outside of the IRQ so the IRQ is a tiny bit faster
;  DECB                     * Seems to draw it one row too low, this should fix it
  STA     ColourTransformer_1+2        * Save A in selfmod ADDD below
  LDA     #160             * D = B * 160 (Y co-ordinate * 160)
  MUL
ColourTransformer_1:
  ADDD    #$0000           * Self modified from above
  STD     4,X              * Write out CoCo 3 screen address for this Transporter to be drawn
  PULS  B                  * Get the colour to be used for this sprite
  CMPB   #$DD              * is it grey = Normal colour?
  BNE    >
  LDD   #TRANSPORTERS      * point to normal Transporter Colour of grey
  BRA   ColourTransformer_5
!
  CMPB   #$11              * is it white = Enemy
  BNE    >
  LDD   #Transporter_White * point to Transporter with the Enemy Colour of white
  BRA   ColourTransformer_5
!
  CMPB   #$55              * is it Player 1's colour?
  BNE   >
  LDD   #Transporter_Yellow * point to Transporter with Player 1's colour of Yellow
  BRA   ColourTransformer_5
!
  LDD   #Transporter_Blue  * point to Transporter with Player 1's colour of Blue
ColourTransformer_5:
  STD    2,X               * Save the address of the Transporter compiled sprite code
  LDA    #%01001111        * Use this as a flag to indicate it's a coloured Transporter to draw
  LDB    #Sprites01        * We will use 1,X as the info for which MMU block to use for the sprites
  BRA     L049_001A
  ENDIF


  IF LavaBubble
*
*	DO A "BUBBLE" OR A BUBBLE THAT IS IN MIDDLE OF THE LAVA
*
BubbleX   RMB   2
BubbleRememberMMU   RMB  1
MyBubble:
* Set lower RAM to screen mode
    LDB     MMU_Reg_Bank0_4
    STB     LavaRestoreMMU1+1
		LDB   #4
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 - Graphics RAM bank
	LDB	PFRAME-2,U
	ANDB	#$0F		limits bubbles to range E2 to EF
	ORB	#$02		so that they stay in the lava
	ADDB	SAFRM2+1	USE INTERNAL LIMITS, NOT SAFRAM
	INCB
	TFR	D,X
	STX	PFRAME+4,U
    STA   LavaBubble1+2   * Save A in selfmod ADDD below
    LDA   #160                * D = B * 160 (Y co-ordinate * 160)
    MUL
LavaBubble1:
    ADDD  #$0000              * Self modified from above
    TFR   D,X             * X has our CoCo 3 Joust screen position
	LDA	#CLR3*LN
; STA ,X
	STA	,X
  STX BubbleX       * Save X
	LDX	#SNBUB		THE SOUND A BUBBLE MAKES
	JSR	SND
* Put blocks back to normal
* Set lower RAM to screen mode
LavaRestoreMMU1:
		LDB     #$FF             * Self mod from above
    STB     MMU_Reg_Bank0_4  * Set Bank 4
	PCNAP	8
* Set lower RAM to screen mode
    LDB     MMU_Reg_Bank0_4
    STB     LavaRestoreMMU2+1
		LDB   #4
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 - Graphics RAM bank
;	LDX	PFRAME+4,U
  LDX BubbleX      * Get X
	LDA	#CLR3*RN	$03
;	STA	,X
  STA   ,X
;	STA	$100,X
  STA   1,X
	LDA	#CLR3*LN	$30
;	STA	-1,X
  STA   -160,X
;	STA	1,X
  STA   160,X
* Restore back to normal lower RAM to screen mode
LavaRestoreMMU2:
		LDB     #$FF             * Self mod from above
    STB     MMU_Reg_Bank0_4  * Set Bank 4
	PCNAP	8
* Set lower RAM to screen mode
    LDB     MMU_Reg_Bank0_4
    STB     LavaRestoreMMU3+1
		LDB   #4
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 - Graphics RAM bank
;	LDX	PFRAME+4,U
  LDX BubbleX      * Get X
	LDA	#CLR3*BN	$33
;	STA	-1,X
  STA   -160,X
;	STA	,X
  STA   ,X
;	STA	1,X
  STA   160,X
;	STA	$100,X
  STA   1,X
	LDA	#5
	ADDA	PFRAME-2,U	This rolls the height of the bubbles
	STA	PFRAME-2,U
* Restore back to normal lower RAM to screen mode
LavaRestoreMMU3:
		LDB     #$FF             * Self mod from above
    STB     MMU_Reg_Bank0_4  * Set Bank 4
  RTS
*
*	DO A "BURSTER" OR A BUBBLE THAT IS ON THE SURFACE OF THE LAVA
*
MyBuster:
* Set lower RAM to screen mode
    LDB     MMU_Reg_Bank0_4
    STB     LavaRestoreMMU4+1
		LDB   #4
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 - Graphics RAM bank
	LDB	SAFRM2+1
	INCB
	STD	PFRAME+4,U
    STA   LavaBubble2+2   * Save A in selfmod ADDD below
    LDA   #160                * D = B * 160 (Y co-ordinate * 160)
    MUL
LavaBubble2:
    ADDD  #$0000              * Self modified from above
    TFR   D,X             * X has our CoCo 3 Joust screen position
  STX BubbleX       * Save X
;	CLR	1,X
  CLR 160,X
* Restore back to normal lower RAM to screen mode
LavaRestoreMMU4:
		LDB     #$FF             * Self mod from above
    STB     MMU_Reg_Bank0_4  * Set Bank 4
	PCNAP	8
* Set lower RAM to screen mode
    LDB     MMU_Reg_Bank0_4
    STB     LavaRestoreMMU5+1
		LDB   #4
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 - Graphics RAM bank
;	LDX	PFRAME+4,U
  LDX BubbleX      * Get X
	LDB	#CLR3*BN
;	STB	1,X
  STB 160,X
;	CLR	,X
  CLR ,X
;	STB	-1,X
  STB -160,X
* Restore back to normal lower RAM to screen mode
LavaRestoreMMU5:
		LDB     #$FF             * Self mod from above
    STB     MMU_Reg_Bank0_4  * Set Bank 4
	PCNAP	8
* Set lower RAM to screen mode
    LDB     MMU_Reg_Bank0_4
    STB     LavaRestoreMMU6+1
		LDB   #4
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 - Graphics RAM bank
;	LDX	PFRAME+4,U
  LDX BubbleX      * Get X
	LDB	#CLR3*BN
;	STB	,X
  STB ,X
;	STB	-$101,X
  STB -161,X
;	STB	$FF,X
  STB -159,X
;	CLR	-1,X
  CLR  -160,X
* Restore back to normal lower RAM to screen mode
LavaRestoreMMU6:
		LDB     #$FF             * Self mod from above
    STB     MMU_Reg_Bank0_4  * Set Bank 4
	PCNAP	8
* Set lower RAM to screen mode
    LDB     MMU_Reg_Bank0_4
    STB     LavaRestoreMMU7+1
		LDB   #4
    STB     MMU_Reg_Bank0_4  	* Set Bank 4 - Graphics RAM bank
;	LDX	PFRAME+4,U
  LDX BubbleX      * Get X
;	CLR	-$101,X
  CLR -161,X
;	CLR	$FF,X
  CLR -159,X
* Restore back to normal lower RAM to screen mode
LavaRestoreMMU7:
		LDB     #$FF             * Self mod from above
    STB     MMU_Reg_Bank0_4  * Set Bank 4
  RTS
  ENDIF



ErrorLoop:
    ORCC  #$50
!
    BRA   <

* Memory space that can be used for my code:
* From    To
* E5C4    E697  - Just after SOUND TABLE FOR THE MODULE - SNBUB

  END	CoCo_START
                 
