!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!MIT License
!
!Copyright (c) Dr. Dr. Danny E.P. Vanpoucke, https://dannyvanpoucke.be
!
!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:
!
!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.
!
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!>
!! \brief Module containing the \link ttimer TTimer class\endlink for practical timing.
!!
!! @author  Dr. Dr. Danny E. P. Vanpoucke
!! @version 2.0-3  (upgrades deprecated timing module)
!! @date    19-03-2020
!! @copyright https://dannyvanpoucke.be
!!
!! This module makes use of:
!! - \link timeclass TimeClass \endlink
!<-----------------------------------------------------------------------
module Timerclass
    use TimeClass
    implicit none
    private

    !++++++++++++++++++++++++++++++++++++++++++
    !> \class ttimer
    !! \brief A class for keeping track of time
    !! in calculations, by creating a list of timestamps.
    !<----------------------------------------
    type, public :: TTimer
        private
        !! Some EXTRA INFO on \ref ttimer
        integer :: ntimes   !< @private The number of timestamps stored
        integer :: maxtimes !< @private The size of the timestamp list
        Type(TTime), allocatable :: timestamps(:)!< @private Allocatable list of timestamps
        logical, allocatable :: timedInterval(:) !< @private logical indicating if the timer was running
                                                 !< during an interval between two timestamps or not
        logical :: running  !< @private True if the timer is running
        logical :: paused   !< @private True if the timer is paused
        logical :: stopped  !< @private True if the timer is stopped (final end)
    contains
        private
        procedure, pass(this), public :: Start       !< @public @copydoc timerclass::start
        procedure, pass(this), public :: Interrupt   !< @public @copydoc timerclass::interrupt
        procedure, pass(this), public :: Resume      !< @public @copydoc timerclass::resume
        procedure, pass(this), public :: AddTimeFlag !< @public @copydoc timerclass::addtimeflag
        procedure, pass(this), public :: StopTimer   !< @public @copydoc timerclass::stoptimer
        procedure, pass(this), public :: Reset       !< @public @copydoc timerclass::reset
        procedure, pass(this), public :: PrintElapsedTimeReport!< @public @copydoc timerclass::printelapsedtimereport
        procedure, pass(this), public :: GetElapsedTimeString  !< @public @copydoc timerclass::getelapsedtimestring
        procedure, pass(this)         :: GetElapsedTime_total  !< @private @copydoc timerclass::getelapsedtime_total
        procedure, pass(this)         :: GetElapsedTime_steps  !< @private @copydoc timerclass::getelapsedtime_steps
        procedure, pass(this)         :: Copy         !< @private @copydoc timerclass::copy
        procedure, pass(this),        :: AddTimestamp !< @private @copydoc timerclass::addtimestamp
        generic, public :: assignment(=) => Copy      !< @public  @copydoc timerclass::copy
        generic, public :: GetElapsedTime => GetElapsedTime_total, GetElapsedTime_steps !< @public Generic interface to the
                                                        !< \link timerclass::getelapsedtime_total GetElapsedTime_total\endlink
                                                        !< and \link timerclass::getelapsedtime_steps GetElapsedTime_steps\endlink methods of
                                                        !< the \link ttimer TTimer\endlink class.
        !> @{ @protected
        final :: destructor !< @copydoc timerclass::destructor
        !> @}
    end type TTimer


    ! This is the only way a constructor can be created, as no "initial" exists, emulates the C++ constructor behaviour
    interface TTimer
        module procedure Constructor
    end interface TTimer

contains
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Constructor for the \link ttimer TTimer\endlink instances.
    !!
    !! \b Usage:
    !! \code{.f03}
    !! Type(TTimer) :: T
    !! T=TTimer()
    !! \endcode
    !!
    !! \return Returns a \link ttimer TTimer\endlink object
    !<-------------------------------------------
    pure function Constructor()Result(Timer)
        Type(TTimer) :: Timer

    Timer%ntimes=0
    Timer%maxtimes=10
    allocate(Timer%timestamps(1:10))
    allocate(Timer%timedInterval(1:10))
    Timer%timedInterval(:)=.false.
    Timer%running=.false.
    Timer%paused=.false.
    Timer%stopped=.false.

    end function Constructor
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Start the \link ttimer TTimer\endlink instance (clean start, everything is reset).
    !! If the timer was already running it is reset first.
    !!
    !! @param[in,out] this The \link ttimer TTimer\endlink instance.
    !! \return TS The index of the first timestamp.
    !<-------------------------------------------
    function Start(this) Result(TS)
        class(TTimer), intent(inout) :: this
        integer :: TS

        if (this%running) call this%reset()
        this%running=.true.
        TS=this%AddTimestamp()
        this%ntimes=TS
        this%timedInterval(TS)=.true. ! this timer-interval is accounted

    end function Start
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Restart the \link ttimer TTimer\endlink instance after a pause. If the timer is not paused,
    !! nothing will happen and TS=-1 is returned.
    !!
    !! @param[in,out] this The \link ttimer TTimer\endlink instance.
    !! \return TS The index of the first timestamp.
    !<-------------------------------------------
    function Resume(this) Result(TS)
        class(TTimer), intent(inout) :: this
        integer :: TS

        TS=-1
        if (this%paused) then
            this%running=.true.
            this%paused=.false.
            TS=this%AddTimestamp()
            this%ntimes=TS
            this%timedInterval(TS)=.true. ! this timer-interval is accounted
        end if

    end function Resume
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Add an additional timestamp without changing the
    !! status (running/paused) of the \link ttimer TTimer\endlink instance.
    !!
    !! If the timer is stopped nothing will happen, and -1 is returned.
    !!
    !! @param[in,out] this The TTimer instance.
    !! \return TS The index of the timestamp.
    !<-------------------------------------------
    function AddTimeFlag(this) Result(TS)
        class(TTimer), intent(inout) :: this
        integer :: TS

        TS=-1
        if (this%running.or.this%paused) then
            TS=this%AddTimestamp()
            this%ntimes=TS
            this%timedInterval(TS)=this%running ! is this timer-interval accounted?
        end if

    end function AddTimeFlag
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Puts the \link ttimer TTimer\endlink instance on hold.
    !!
    !! If the timer was not running nothing will happen.
    !! The optional IO parameter will return an error code.\n
    !! <b>IO values:</b>
    !!  - 0  : all is well
    !!  - -1 : The timer was not running, so nothing to pause.
    !!  - -2 : The timer was already stopped/paused, so nothing to pause.
    !!
    !! In case of error, TS will be set to -1.
    !!
    !! @param[in,out] this The \link ttimer TTimer\endlink instance.
    !! @param[out] IO Optional parameter giving the error-status. [\b OPTIONAL ; \b DEFAULT= 0]
    !! \return TS The index of the final timestamp.
    !<-------------------------------------------
    function Interrupt(this,IO) Result(TS)
        class(TTimer), intent(inout) :: this
        integer, intent(out), optional :: IO
        integer :: TS

        TS=-1
        if (this%running) then
            if ((.not.(this%stopped)).and.(.not.(this%paused))) then
                if (present(IO)) IO=0
                TS=this%AddTimestamp()
                this%paused=.true.
                this%timedInterval(TS)=.false. ! as the timer is pauzed, the following interval should not be accounted
            else
                if (present(IO)) IO=-2
            end if
        else
            if (present(IO)) IO=-1
        end if

    end function Interrupt
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Stops the \link ttimer TTimer\endlink instance (terminal fashion...no restart possible).
    !!
    !! If the timer was not running nothing will happen.
    !! The optional IO parameter will return an error code.\n
    !! <b>IO values:</b>
    !!  - 0  : all is well
    !!  - -1 : The timer was not running, so nothing to stop.
    !!  - -2 : The timer was already stopped, so nothing to stop.
    !!
    !! In case of error, TS will be set to -1.
    !!
    !! @param[in,out] this The \link ttimer TTimer\endlink instance.
    !! @param[out] IO Optional parameter giving the error-status. [\b OPTIONAL ; \b DEFAULT= 0]
    !! \return TS The index of the final timestamp.
    !<-------------------------------------------
    function StopTimer(this,IO) Result(TS)
        class(TTimer), intent(inout) :: this
        integer, intent(out), optional :: IO
        integer :: TS

        TS=-1
        if (this%running) then
            if (.not.(this%stopped)) then
                if (present(IO)) IO=0
                TS=this%AddTimestamp()
                this%stopped=.true.
                this%timedInterval(TS)=.false. ! as the timer is stopped, the following interval should not be accounted
            else
                if (present(IO)) IO=-2
            end if
        else
            if (present(IO)) IO=-1
        end if

    end function StopTimer
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Start the \link ttimer TTimer\endlink instance.
    !! If the timer was already running it is reset first.
    !!
    !! @param[in,out] this The \link ttimer TTimer\endlink instance.
    !<-------------------------------------------
    pure subroutine Reset(this)
        class(TTimer), intent(inout) :: this

        this%ntimes=0
        this%maxtimes=10
        if (allocated(this%timestamps)) deallocate(this%timestamps)
        allocate(this%timestamps(1:10))
        if (allocated(this%timedInterval)) deallocate(this%timedInterval)
        allocate(this%timedInterval(1:10))
        this%timedInterval(:)=.false.
        this%running=.false.
        this%paused=.false.
        this%stopped=.false.

    end subroutine Reset
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Add a timestamp to a running \link ttimer TTimer\endlink instance,
    !! returning the index of the timestamp.
    !!
    !! In case the timer is not running, nothing is done and -1 is returned.
    !!
    !! @param[in,out] this The \link ttimer TTimer\endlink instance.
    !! \return TS The index of the timestamp.
    !<-------------------------------------------
    function AddTimestamp(this)Result(TS)
        class(TTimer), intent(inout) :: this
        integer:: TS

        type(TTime), allocatable :: tmp(:)
        logical, allocatable :: tmpL(:)

        if (this%running) then
            TS=this%ntimes+1
            if (TS>this%maxtimes) then ! we need to extend the arrays
                allocate(tmp(1:this%maxtimes))
                allocate(tmpL(1:this%maxtimes))
                tmp(1:this%maxtimes)=this%timestamps(1:this%maxtimes)
                tmpL(1:this%maxtimes)=this%timedInterval(1:this%maxtimes)
                this%maxtimes=this%maxtimes+10
                if (allocated(this%timestamps)) deallocate(this%timestamps)
                allocate(this%timestamps(1:this%maxtimes))
                if (allocated(this%timedInterval)) deallocate(this%timedInterval)
                allocate(this%timedInterval(1:this%maxtimes))
                this%timestamps(1:this%maxtimes-10)=tmp(1:this%maxtimes-10)
                this%timedInterval(1:this%maxtimes-10)=tmpL(1:this%maxtimes-10)
                this%timedInterval(this%maxtimes-10:this%maxtimes)=.false.
                deallocate(tmp)
                deallocate(tmpL)
            end if
            this%timestamps(TS)=TTime()
            call this%timestamps(TS)%SetTime()
            this%ntimes=TS
        else
            TS=-1
        end if

    end function AddTimestamp
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Returns the number of seconds which elapsed between the start and stop timestamps.
    !!
    !! @param[in] this The \link ttimer TTimer\endlink instance.
    !! @param[in] INCL_PAUSE Logical indicating if the time during which the timer
    !!             was paused should be included as well. [\b OPTIONAL, \b DEFAULT = .false. ]
    !! \return Seconds The (fractional) number of seconds elapsed, as double precision real.
    !<-------------------------------------------
    pure function GetElapsedTime_total(this,INCL_PAUSE) Result(sec)
    use, intrinsic :: iso_fortran_env
        class(TTimer), intent(in) :: this
        logical, intent(in), optional :: INCL_PAUSE
        integer, parameter :: dp = REAL64
        real(dp) :: sec

        logical :: pauze

    pauze=.false.
    if (present(INCL_PAUSE)) pauze=INCL_PAUSE

    sec=this%GetElapsedTime_steps(1,this%maxtimes,INCL_PAUSE=pauze)

    end function GetElapsedTime_total
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Returns the number of seconds which elapsed between two timestamps.
    !! This is always a positive value.
    !!
    !! \b NOTE:
    !! - If the timestamps are out of range, then -1 is returned.
    !! - If the start comes after end, then they are exchanged such that a positive time is obtained.
    !!
    !! @param[in] this The \link ttimer TTimer\endlink instance.
    !! @param[in] Tstart, Tend The indices of the start and end time.
    !! @param[in] INCL_PAUSE Logical indicating if the time during which the timer was paused should be included as well. [\b OPTIONAL, \b DEFAULT = .false. ]
    !! \return Seconds The (fractional) number of seconds elapsed, as double precision real.
    !<-------------------------------------------
    pure function GetElapsedTime_steps(this,Tstart,Tend,INCL_PAUSE) Result(sec)
    use, intrinsic :: iso_fortran_env
        class(TTimer), intent(in) :: this
        integer, intent(in) :: Tstart, Tend
        logical, intent(in), optional :: INCL_PAUSE
        integer, parameter :: dp = REAL64
        real(dp) :: sec

        type(TTime) :: elap, tmp
        integer :: T1, T2, nr

    if ((Tstart<1).or.(Tend<1).or.(Tstart>this%ntimes).or.(Tend>this%ntimes)) then
        sec=-1.0
        return
    end if

    if (Tstart>Tend) then
        T1=Tend
        T2=Tstart
    else
        T1=Tstart
        T2=Tend
    end if

    elap=this%timestamps(T2)-this%timestamps(T1)
    sec=elap%GetTimeSeconds()
    if (present(INCL_PAUSE)) then
        if (INCL_PAUSE) then ! pauses should be excluded, so we subtract the again
            do nr=1,this%ntimes-1
                if (.not.this%timedInterval(nr)) then
                    tmp=this%timestamps(nr+1)-this%timestamps(nr)
                    sec=sec-tmp%GetTimeSeconds()
                end if

            end do
        end if
    end if

    end function GetElapsedTime_steps
    !++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Returns a string representing the elapsed time.
    !!+
    !! @param[in] this The \link ttimer TTimer\endlink instance.
    !! @param[in] TSTART, TSTOP Indices of the start and end-times. [\b OPTIONAL, \b DEFAULT: TSTART=1, TSTOP=index of last timestamp]
    !! @param[in] INCL_PAUSE Logical indicating if the time during which the timer was paused should be included as well. [\b OPTIONAL, \b DEFAULT = .false. ]
    !! @param[in] FMT string indicating the format for the time-string.
    !!            - sec : xxx.xxx secs
    !!            - hms : xxxx h xx min xx.xxx secs
    !!            - dhms: xx days xx h xx min xx.xxx secs
    !!            - hour: xxx.xxx hours
    !!            - day : xxx.xxx days
    !!            [\b OPTIONAL, \b DEFAULT= sec]
    !! \return str The string with the formatted time.
    !<---------------------------------------------
    pure function GetElapsedTimeString(this,TSTART, TSTOP, INCL_PAUSE, FMT) Result(str)
    use, intrinsic :: iso_fortran_env
        class(TTimer), intent(in) :: this
        integer, intent(in), optional :: TSTART, TSTOP
        logical, intent(in), optional :: INCL_PAUSE
        character(len=*), intent(in), optional :: FMT
        character(len=50) :: str

        integer, parameter :: dp = REAL64
        real(dp) :: sec
        integer :: T1, T2, nr, hour, day, minute
        character(len=4) :: opt
        character(len=255) :: line

        T1=1
        T2=this%ntimes
        if(present(TSTART)) then
            if ((TSTART>0).and.(TSTART<=this%ntimes)) T1=TSTART
        end if
        if(present(TSTOP)) then
            if ((TSTOP>0).and.(TSTOP<=this%ntimes)) T2=TSTOP
        end if
        if (T1>T2) then
            nr=T1
            T1=T2
            T2=nr
        end if

        sec=this%GetElapsedTime(T1,T2,INCL_PAUSE)
        !now transform to a string
        opt='sec'
        if (present(fmt)) opt=trim(adjustl(fmt))

        select case(trim(adjustl(opt)))
            case ('sec')
                write(line,'(F30.3,A)') sec," secs "
            case ('hour')
                write(line,'(F30.3,A)') sec/3600.0_dp," hours "
            case ('day')
                write(line,'(F30.3,A)') sec/86400.0_dp," days "
            case ('hms')
                hour=FLOOR(sec/3600.0_dp)
                sec=sec-(hour*3600.0_dp)
                minute=FLOOR(sec/60.0_dp)
                sec=sec-(minute*60.0_dp)
                write(line,'(I0,A,I2,A,F6.3)') hour," h ",minute," min ",sec," secs "
            case ('dhms')
                day=FLOOR(sec/86400.0_dp)
                sec=sec-(day*864000_dp)
                hour=FLOOR(sec/3600.0_dp)
                sec=sec-(hour*3600.0_dp)
                minute=FLOOR(sec/60.0_dp)
                sec=sec-(minute*60.0_dp)
                write(line,'(I0,A,2(I2,A),F6.3)') day," days ",hour," h ",minute," min ",sec," secs "
        end select
        write(str,'(3A)') " ",trim(adjustl(line))," "

    end function GetElapsedTimeString
    !++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Print small timings report to unit UN.
    !!
    !!
    !! @param[in] this The \link ttimer TTimer\endlink instance.
    !! @param[in] message String containing a message to include.
    !! @param[in] Tstart,Tstop Integer indexes of the start and stop times
    !! @param[in] UN Integer indicating the unit to write the report to.
    !! @param[in] INCL_PAUSE Logical indicating if the time during which the timer was paused should be included as well. [\b OPTIONAL, \b DEFAULT = .false. ]
    !<---------------------------------------------
    subroutine PrintElapsedTimeReport(this, message, Tstart, Tstop, UN, INCL_PAUSE)
    use, intrinsic :: iso_fortran_env
        class(TTimer), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in) :: Tstart, Tstop
        integer, intent(in) :: UN
        logical, intent(in), optional :: INCL_PAUSE

        character(len=50) :: line
        integer, parameter :: dp = REAL64
        real(dp) :: sec
        integer :: ih, im

        write(UN,"(2A)",advance='NO') trim(message)," : "
        line=this%GetElapsedTimeString(Tstart,Tstop,INCL_PAUSE,'sec')
        write(UN,"(A)") trim(adjustl(line))
        sec=this%GetElapsedTime(Tstart,Tstop,INCL_PAUSE)

        ih=FLOOR(sec/3600.0_dp)
        sec=sec-dble(ih)*3600.0_dp
        im=FLOOR(sec/60.0_dp)
        sec=sec-dble(im)*60.0_dp

        write (UN,'(I8,A)') ih," hours"
        write (UN,'(I8,A)') im," minutes"
        write (UN,'(F8.3,A)') sec," seconds"

    end subroutine PrintElapsedTimeReport
    !++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Function to copy one \link ttimer TTimer\endlink instance to the current one via the "=" assignment.
    !!
    !! @param[in,out] this The \link ttimer TTimer\endlink instance before the "=" assignment.
    !! @param[in] from The \link ttimer TTimer\endlink instance after the "=" assignment.
    !<---------------------------------------------
    pure subroutine Copy(this,from)
        class(TTimer), intent(inout) :: this
        class(TTimer), intent(in) :: from

    integer :: nr

    this%maxtimes=from%maxtimes
    this%ntimes=from%ntimes
    this%paused=from%paused
    this%running=from%running
    this%stopped=from%stopped
    allocate(this%timedInterval(1:this%maxtimes))
    this%timedInterval(1:this%maxtimes)=from%timedInterval(1:this%maxtimes)
    allocate(this%timestamps(1:this%maxtimes))
    do nr=1, this%ntimes
        this%timestamps(nr)=from%timestamps(nr)
    end do

    end subroutine Copy
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Destructor of the \link ttimer TTimer\endlink class. Cleans up
    !! the instance upon finalization.
    !!
    !! @param[in,out] this The \link ttimer TTimer\endlink instance being destroyed automatically.
    !<-------------------------------------------
    subroutine destructor(this)
        Type(TTimer) :: this

    if (allocated(this%timestamps)) deallocate(this%timestamps)
    if (allocated(this%timedInterval)) deallocate(this%timedInterval)

    end subroutine destructor


end module Timerclass
