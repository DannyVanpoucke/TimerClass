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
!> \brief The <b>TimeClass module</b> contains the \link ttime TTime class\endlink used by the
!! \link timerclass::ttimer TTimer class\endlink for practical timing.
!!
!! @author  Dr. Dr. Danny E. P. Vanpoucke
!! @version 2.0-3  (upgrades deprecated timing module)
!! @date    19-03-2020
!! @copyright https://dannyvanpoucke.be
!!
!! @warning Internally, Julian Day Numbers are used to compare dates. As a result, *negative* dates are not accepted.
!! If such dates are created (*e.g.*, due to a subtraction), then the date is set to zero.
!!
!! This module makes use of:
!! - nothing; this module is fully independent
!<-----------------------------------------------------------------------
module TimeClass
    implicit none
    private


    !+++++++++++++++++++++++++++++++++++++++
    !> @class ttime
    !! \brief The TTime class contains all time functionality
    !! with regard to a single time stamp.
    !<-------------------------------------
    type, public :: TTime
      private
        integer :: year    !< @private The year
        integer :: month   !< @private The month (as integer).
        integer :: day     !< @private Day of the month.
        integer(kind=4) :: JDN !< @private The Julian Day Number, as a long-int (4-byte)
        real    :: daysecs !< @private Number of seconds of the day, with millisecond resolution.
    contains
      private
        procedure, pass(this),public :: SetTime       !<          @copydoc timeclass::settime
        procedure, pass(this)        :: CalculateJDN  !< @private @copydoc timeclass::calculatejdn
        procedure, pass(this)        :: SetJDN        !< @private @copydoc timeclass::setjdn
        procedure, pass(this)        :: SetGregorianDateFromJDN !< @private @copydoc timeclass::setgregoriandatefromjdn
        procedure, pass(this),public :: GetJulianDayNumber      !< @copydoc timeclass::getjuliandaynumber
        procedure, pass(this)        :: copy          !< @private @copydoc timeclass::copy
        procedure, pass(this)        :: add           !< @private @copydoc timeclass::add
        procedure, pass(this)        :: subtract      !< @private @copydoc timeclass::subtract
        procedure, pass(this),public :: IsLeapYear    !<          @copydoc timeclass::isleapyear
        procedure, pass(this),public :: GetTimeString !<          @copydoc timeclass::gettimestring
        procedure, pass(this),public :: GetTimeSeconds!<          @copydoc timeclass::gettimeseconds
        generic, public :: assignment(=) => copy      !<          @copydoc timeclass::copy
        generic, public :: operator(+)   => add       !<          @copydoc timeclass::add
        generic, public :: operator(-)   => subtract  !<          @copydoc timeclass::subtract
        !> @{ @protected
        final :: destructor !< @copydoc timeclass::destructor
        !> @}
    end type TTime

    ! This is the only way a constructor can be created, as no "initial" exists, emulates the C++ constructor behaviour
    interface TTime
        module procedure constructor
    end interface TTime

contains
    !+++++++++++++++++++++++++++++++++++++++++
    !>\brief Constructor for the \link ttime TTime\endlink class.
    !!
    !! \note This constructor does not set the time. It just enters zero's.
    !!
    !! \b usage:
    !! \code{.f03}
    !! Type(TTime) :: T
    !! T = TTime()
    !! \endcode
    !!
    !! \return Time An instance of the TTime class.
    !<-----------------------------------------
    pure function constructor()Result(Time)
        type(TTime) :: Time

        Time%year=0
        Time%month=0
        Time%day=0
        Time%JDN=0
        Time%daysecs=0
    end function constructor
    !++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Function to set the \link ttime TTime\endlink instance to the current time,
    !! with millisecond resolution.
    !!
    !! \ingroup ttime
    !! \note As this subroutine uses the date_and_time intrinsic it is an *impure* subroutine.
    !!
    !! @param[in,out] this The \link ttime TTime\endlink instance being called.
    !<---------------------------------------------
    subroutine SetTime(this)
        class(TTime), intent(inout) :: this
        integer time_array(8)

        call date_and_time(values=time_array) ! this function seems to be impure
        this%year=time_array(1)
        this%month=time_array(2)
        this%day=time_array(3)
        this%daysecs=(((real(time_array(5))*60.0 + real(time_array(6)))*60.0)+real(time_array(7)))&
                    & + 0.001*real(time_array(8))
        ! Transform the Gregorian date into a Julian Day Number
        call this%CalculateJDN()

    end subroutine SetTime
    !+++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Calculating the Julian Day number based
    !! on the Gregorian date set in the \link ttime TTime\endlink object.
    !!
    !! In practice the Gregorian calender date set in the \link ttime TTime\endlink
    !! instance is transformed into a Julian Day Number, and stored in the instance as \ref ttime::jdn.
    !!
    !! @param[in,out] this The \link ttime TTime\endlink instance being called.
    !<----------------------------------------------
    pure subroutine CalculateJDN(this)
        class(TTime), intent(inout) :: this

        this%JDN=INT((1461*(this%year+4800+INT((this%month-14)/12)))/4)+&
                &INT((367*(this%month-2-12*INT((this%month-14)/12)))/12)-&
                &INT((3*INT((this%year+4900+INT((this%month-14)/12))/100))/4)&
                &+this%day-32075

    end subroutine CalculateJDN
    !+++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Set the Julian Day Number of a \link ttime TTime\endlink instance manually.
    !!
    !! \note The Julian Day Number should be >=0.
    !! For negative values it is set to 0, and an error value is set to IO
    !!
    !! @param[in,out] this The \link ttime TTime\endlink instance being called.
    !! @param[in] JDN A positive integer(kind=4) value representing a valid Julian Day Number.
    !! @param[out] IO Integer value returning 0 upon success, and a negative value(=JDN) in case of failure.
    !<----------------------------------------------
    pure subroutine setJDN(this,JDN,IO)
        class(TTime), intent(inout) :: this
        integer(kind=4), intent(in) :: JDN
        integer, intent(out), optional :: IO

        if (JDN>=0) then
            this%JDN=JDN
        else
            this%JDN=0
        end if
        call this%SetGregorianDateFromJDN()

        if (present(IO)) then
            IO=0
            if (this%JDN<0) IO=-1
        end if

    end subroutine setJDN
    !+++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Function returning the Julian Day Number as a 4-byte integer.
    !!
    !! Use integer(kind=4).
    !!
    !! @param[in,out] this The \link ttime TTime\endlink instance being called.
    !! \return JDN The integer Julian Day Number.
    !<----------------------------------------------
    pure function GetJulianDayNumber(this) Result(JDN)
        class(TTime), intent(in) :: this
        integer(kind=4) :: JDN

        JDN=this%JDN

    end function GetJulianDayNumber
    !+++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Subroutine which transforms the set Julian Day Number into a Gregorian Calender date.
    !!
    !! \note The routine is only valid for a JDN>=0.
    !!
    !! @param[in,out] this The \link ttime TTime\endlink instance being called.
    !! @param[out] IO Returns 0 on success, and -1 for failure. [\b OPTIONAL ]
    !<----------------------------------------------
    pure subroutine SetGregorianDateFromJDN(this, IO)
        class(TTime), intent(inout) :: this
        integer,intent(out),optional :: IO

        integer(kind=4),parameter :: y=4716, j=1401, m=2, n=12, r=4, p=1461, v=3, u=5, s=153, w=2, B=274277, C=-38
        integer(kind=4) :: f, e, g, h

        if (present(IO)) then
            IO=0
            if (this%JDN<0) IO=-1
            if (IO<0) return
        end if
        f=this%JDN + j +INT((INT((4*this%JDN + B)/146097)*3)/4) + C
        e=r*f + v
        g=INT(mod(e,p)/r)
        h=u*g+w
        this%day=INT((mod(h,s))/u)+1
        this%month=mod(INT(h/s)+m,n)+1
        this%year=INT(e/p)-y+INT((n+m-this%month)/n)

    end subroutine SetGregorianDateFromJDN
    !++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Function to copy one \link ttime TTime\endlink instance
    !! to the current one via the "=" assignment. This is the function
    !! performing the actual operator overloading.
    !!
    !! \b usage:
    !! \code{.f03}
    !! Tnew = Told
    !! \endcode
    !!
    !! @param[in,out] this The \link ttime TTime\endlink instance before the "=" assignment.
    !! @param[in] from The \link ttime TTime\endlink instance after the "=" assignment.
    !<---------------------------------------------
    pure subroutine copy(this,from)
        class(TTime), intent(inout) :: this
        class(TTime), intent(in) :: from

        this%year=from%year
        this%month=from%month
        this%day=from%day
        this%daysecs=from%daysecs
        this%JDN=from%JDN

    end subroutine copy
    !++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Function to add two \link ttime TTime\endlink instance
    !! via the "+" operator. This is the function
    !! performing the actual operator overloading.
    !!
    !! Adding two full dates is maybe a bit strange to do. In our case, we don't
    !! just add the days and add the months, but we add the days of the year (via their Julian Day Numbers)
    !! and transform these back to months and days. (why keep life simple if we can
    !! make it complicated?)
    !!
    !! \b usage:
    !! \code{.f03}
    !! Total = this + that
    !! \endcode
    !! This line also calls the \link copy assignment operator\endlink.
    !!
    !! @param[in] this The \link ttime TTime\endlink instance before the "+" operator.
    !! @param[in] that The \link ttime TTime\endlink instance after the "+" operator.
    !! \return Total The \link ttime TTime\endlink instance representing the sum.
    !<---------------------------------------------
    pure function add(this,that) Result(Total)
        class(TTime), intent(in) :: this, that
        Type(TTime) :: total

        integer :: overflow, ios
        integer(kind=4) :: days

		total = TTime()
        total%daysecs=this%daysecs+that%daysecs
        overflow=0
        if (total%daysecs>60.0) then
            overflow=int((total%daysecs - modulo(total%daysecs,60.0))/60.0)
            total%daysecs=modulo(total%daysecs,60.0)
        end if
        ! now using Julian Day Numbers:
        days=this%GetJulianDayNumber()+that%GetJulianDayNumber()+overflow
        call Total%SetJDN(days,IO=ios) ! this also sets the days, months and years

    end function add
    !++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Function to subtract two \link ttime TTime\endlink instance
    !! via the "-" operator. This is the function
    !! performing the actual operator overloading.
    !!
    !! \b usage:
    !! \code{.f03}
    !! Total = this - that
    !! \endcode
    !! This line also calls the \link copy assignment operator\endlink.
    !!
    !! \note The result should remain a positive number.
    !!
    !! @param[in] this The \link ttime TTime\endlink instance before the "-" operator.
    !! @param[in] that The \link ttime TTime\endlink instance after the "-" operator.
    !! \return Total The \link ttime TTime\endlink instance representing the difference.
    !<---------------------------------------------
    pure function subtract(this,that) Result(Total)
        class(TTime), intent(in) :: this, that
        Type(TTime) :: total

        integer(kind=4) :: overflow, days
        integer :: ios
        !Using julian day numbers and day seconds, this gets a bit more simple

		total = TTime()
        total%daysecs=this%daysecs-that%daysecs
        overflow=0
        do while (total%daysecs<0.0)
            overflow=overflow+1
            total%daysecs=total%daysecs+86400.0
        end do
        days=this%GetJulianDayNumber()-that%GetJulianDayNumber()-overflow
        call total%SetJDN(days,ios)

    end function subtract
    !++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Function returning true/false if the year of the \link ttime TTime\endlink instance is a leap year.
    !!
    !! A leap year is a multiple of 4, but not 100, unless 400
    !!
    !! @param[in] this The \link ttime TTime\endlink instance to check the leap-year.
    !! \return Leap Boolean indicating if the year is a leap-year.
    !<---------------------------------------------
    pure function IsLeapYear(this) Result(Leap)
        class(TTime), intent(in) :: this
        logical :: Leap

        Leap=.false.
        if (mod(this%year,4)==0) then
            Leap=.True.
            if (mod(this%year,100)==0) then
                Leap=.False.
                if (mod(this%year,400)==0) Leap=.True.
            end if
        end if

    end function IsLeapYear
    !++++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Returns a string with the time as a string.
    !!
    !! \b Format options for fmt:
    !! - full: dd/mm/yyyy  hh:mm:ss.mmm
    !! - date: dd/mm/yyyy
    !! - time: hh:mm:ss.mmm
    !! - days: Gives the total time in fractional days (best used for time-differences). Uses the Julian Day Number.
    !! - hours: Same as days, but transformed to hours.
    !! - seconds: Same as days, but transformed to seconds.
    !!
    !! @param[in] this The \link ttime TTime\endlink instance transform into a string.
    !! @param[in] fmt String representing the possible formatting. [\b OPTIONAL, \b DEFAULT = full]
    !! \return TS String with formatted time.
    !<---------------------------------------------
    pure function GetTimeString(this,fmt) Result(TS)
    use, intrinsic :: iso_fortran_env
        class(TTime), intent(in) :: this
        character(len=*), intent(in), optional :: fmt
        character(len=255) :: TS

        integer, parameter :: dp = REAL64
        integer:: h, m
        real :: s
        real(dp) :: FullT
        character(len=50) :: fmtstr


        s=mod(this%daysecs,60.0)
        m=mod(INT((this%daysecs-s)/60),60)
        h=INT(this%daysecs/3600)
        fmtstr="full"
        if (present(fmt)) fmtstr=fmt

        select case(trim(adjustl(fmtstr)))
            case('full')
                write(TS,'(2(I2,A),I4,2(A,I2),A,F6.3)') this%day,"/",this%month,"/",this%year,"  ",h,":",m,":",s
            case('date')
                write(TS,'(2(I2,A),I4)') this%day,"/",this%month,"/",this%year
            case('time')
                write(TS,'(2(I2,A),F6.3)') h,":",m,":",s
            case('days')
                FullT=this%GetJulianDayNumber()*1.0_dp + (this%daysecs/86400.0_dp)
                write(TS,'(F20.8,A)') FullT," days"
            case('hours')
                FullT=(this%GetJulianDayNumber()*1.0_dp + (this%daysecs/86400.0_dp))*24.0_dp
                write(TS,'(F20.8,A)') FullT," hours"
            case('seconds')
                FullT=(this%GetJulianDayNumber()*86400.0_dp + this%daysecs)
                write(TS,'(F20.8,A)') FullT," secs"
            case default
                write(TS,'(2(I2,A),I4,2(A,I2),A,F6.3)') this%day,"/",this%month,"/",this%year,"  ",h,":",m,":",s
        end select

    end function GetTimeString
    !++++++++++++++++++++++++++++++++++++++++++++
    !>\brief Function returning the time in (fractional) seconds (double precision).
    !!
    !! @param[in] this The \link ttime TTime\endlink instance.
    !! \return sec total number of seconds representing the "time" as a double precision value.
    !<-------------------------------------------
    pure function GetTimeSeconds(this) Result(sec)
    use, intrinsic :: iso_fortran_env
        class(TTime), intent(in) :: this
        integer, parameter :: dp = REAL64
        real(dp) :: sec

    sec=this%GetJulianDayNumber()*86400.0_dp + this%daysecs

    end function GetTimeSeconds
    !+++++++++++++++++++++++++++++++++++++++++++
    !>\brief Destructor of the \link ttime TTime\endlink object instance.
    !!
    !! This subroutine is automatically called upon
    !! finalization of the instance.
    !!
    !! @param[in,out] this The instance of the \link ttime TTime\endlink class in need of destruction.
    !<------------------------------------------
    subroutine destructor(this)
        type(TTime) :: this

    end subroutine destructor


end module
