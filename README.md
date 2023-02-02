### Latest Version : 2023.02.02 V1.1.6.009
More update information and changlogs are in History.text
 
## SMON relocatable source code
#### With the help of this source code you can build your own personal SMON-version  
- The created versions can be used in different address area of the C64-memory.  
- You can change or remove commands.  
- You can add own commands to your SMON version

## About SMON for C64

SMON was published in 1984-1985 as a basic listing in the German computer magazin 64'er.  
It is still a good and often used tool for C64 users until today in 2022.

### Available version of SMON  
Mainly there are 3 different version on several D64 disc images available, which have some common main function, and different aditional functions.      
The versions so far are:
   
   - #### SMONFx000 (FMON-version)
      This is the initial version of SMON, it contains the basic main commands including the trace commands and the main disc commands.   
      This version was presented the first time as a basic listing in 5 parts in the German computer magazin 64'er from 1984/11 to 1985/04. 
The initial version was written by Norfried Mann und Dietrich Weineck
    
   - #### SMONPx000 (PLUS-version) 
      This version has some extended memory commands, and a command to copy SMON to different memory location. To provide this extended functions, the disc functions were removed.  
      This extension was provided as a basic listing in the German computer magazin 64'er 1985/10. 
      This version was written by Mark Richters
   - #### SMONIx000 (ILOC-version) 
      This version is an extension to show the so called 'illegal opcodes' of the 6510 Microcontroller. To provide this extended functions, the disc functions were removed.  
      This extension was provided as a basic listing in the German computer magazin 64'er 1985/12.   
      It was written by Dietrich Weineck

   Except these 3 version, there is one more extension available, which provides more disc memory commands for the FMON-version, it will replace the trace commands. This extension was also provided as a basic listing in the German computer magazin 64'er 1985/12.  
   This was also written by Dietrich Weineck   
   Here I will call it FCOM-version (extended FloppyCommands)

   You can find more details about the SMON at the [C64-Wiki](https://www.c64-wiki.de/wiki/SMON)


## The reason, to work on the source code
 
I was looking for a SMON version, which can run in the memory area from $8000 to $9000, and has the extended disc function included.
But it seems, that this version is not available. A version, which has the extended disc commands included, cannot freely moved into the address area of $8000, and a version which includes the SMON copy command, is missing the disc commands.   
During my search in the web for a SMON source code, i have found at [https://www.forum64.de](https://www.forum64.de/index.php?thread/56085-smon-source-code/&postID=1347932#post1347932) a link to the [GitHub repository](https://github.com/cbmuser/smon-reassembly) from 'cbmuser', which is marked as Public Domain.  
I had a first look on it, and seems it is a good start for my own project.
So I decided to use this as a base for my work, to create different version of SMON.

## The work on the existing SMON source code, step by step

### 1. Checking the existing source code.     
   The source code is the so called EXT, or PLUS version of SMON, and is allmost perfect.

   So far I found 2 problems in the source code.
    
   #### 1.1 a wrong placed branch label.     
   The label was placed in the source code at the wrong position ($CE89), The label should be placed at ($CEA2), this is corrected now.

   <img width="186" alt="Screenshot 2022-08-28 at 11 52 33" src="https://user-images.githubusercontent.com/97148663/187056676-2eb005ce-b5f2-441f-8e7f-e064174b7447.png">  .  .  .  <img width="185" alt="Screenshot 2022-08-28 at 12 01 05" src="https://user-images.githubusercontent.com/97148663/187056773-70ff0263-2fb9-4ca6-ac6e-610955e62b04.png">

   #### 1.2 a missing byte at the end of the source code.     
   The last byte "0E" was missing in the source code, this byte belongs to the adjustment table for the Y-command

   <img width="336" alt="Screenshot 2022-08-28 at 12 06 00" src="https://user-images.githubusercontent.com/97148663/187056897-18b62625-048e-4600-8fc0-06d796d32106.png">

   The byte at address xE04 was not adjusted by the Y-command. This was fixed by adding the byte $0E at the end of the source code.

(Btw, some of the existing PLUS-version on the disc images are missing this byte at the end too)
### 2. Making SMON freely relocatable

   updated all link addresses, and all offsets to relocatable ones, now the SMON can be compiled for different memory address areas.

### 3. Extending the existing source code (SMONPx000)

   #### added code to the source code as following:
   
   - added source code from FMON-version (disc commands),
   
   - added source code from FCOM-version (extended floppy commands) and
   
   - added source code from ILOC-version (illegal opcodes). 
      
   #### with this extensions it is possible to compile any version of SMON
   
### 4. Comments

   added some comments (if i can find the time, i will maybe continue to work on this)
### 5.  Try-out, and bug fixing

   during try out off the different compiled version, I found a bug which is inside SMON from the beginning. The B-command for creating basic data-lines is only working in the version which is compiled for the address range $C000. Even in the copy (Y) command of the PRO version, this error is existing.
    To fix this error the address LC08D must be defined as relocatable address, and must be aded to the table of the (Y) command.
    This is done in the next step.
    
### 6.  Analyzing and improve the (Y) command

   I have analyzed and partly rebuild the move (Y) command. The command now also includes the missing address of the B-command.
   The SMON-PLUS can now be coppied with the Y-command to different memory location as fully working version.
   The B-command to create basic data-lines is fixed and fully working now in the copied version.
   
### 7.  Error correction

   Based on the instructions given in the German magazine 64'er 12/85 some errors are corrected in different places of the source code.
    
## How to use the SMON relocatable multi source code
The source code which is provided here in this repository is now a multi function source code, which can build all different version for different memory adresses.

### How to select the version
The version can be easy selected by uncommenting the relevant part in the sourc code.
in the following example, the "FCOM" part is selected for the build 
        
      ; -----------------------------------------------------------
      ; ---------------------- SMON VERSION -----------------------
      ; ----------------------------------------------------------- 
      ; - DEFINE ONLY ONE VERSION BY UNCOMMENTING IT -
      ; if nothing is selected here, SMON will be compiled without any extension

      ;FMON = 1        ; this is the FMON version, also named SMONFx000, this seems to be the 'normal' or initial one, including the base disc commands
      ;PLUS = 1        ; this is the PLUS version, also named SMONPx000, for the new function the FMON monitor is removed
      ;ILOC = 1        ; this is the ILOC version, also named SMONIx000, this provides the function to show the illegal opcodes, the FMON is removed
      FCOM = 1        ; this is the FCOM version, can named as SMONDx000, this has the extended Floppy cpmmands, the "Trace" function is removed 

Only one version can be used, otherwise SMON will not work correctly.

### How to define the start address
the start address can be defind with the common command like like "*=$C000"  
The start address should be an even 4k address, like: $1000, $4000, aso.   
it is possible to use an address like $6500,    
but with this kind of address, the Y-command in the PLUS-version cannot work correct.

### T-command on different CIA version

On different C64 can be installed different CIA chips. The trace command is using the timer function of the CIA. If the command is not working on your C64, this can be adjusted by selecting the right CIA timing version in the source.

      ; -----------------------------------------------------------
      ; ----------------------- CIA VERSION -----------------------
      ; -----------------------------------------------------------
      ; the new CIA is defined as standard
      ; commenting CIA_N will activate the old version
      ; changing the CIA timing is necessary if the trace-command is not working well
      ; it can also defined manually by changing the value at address $CD8E
       CIA_N = 1     ; change here
      ; -----------------------------------------------------------

## REMARK:
Used Software:    
Visual Studio Code, Version: 1.63.2 (Universal)    
Acme Cross-Assembler for VS Code (c64) v0.0.18  

Used Hardware:    
Apple iMac (24-inch, M1, 2021)      
macOS Ventura, version 13.0 Beta (22A5331f)


The source code can be compiled by using the Acme Cross Compiler (C64)

Please use this source code on your own risk ;)
