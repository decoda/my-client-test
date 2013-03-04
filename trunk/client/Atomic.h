#ifndef Atomic_h__
#define Atomic_h__

namespace Threading
{
	/////////////////////////////////////////////////
	//class AtomicULong
	//  Stores an unsigned long atomically.
	//  Base class for all Arcemu atomics.
	//
	////////////////////////////////////////////////
	class AtomicULong
	{
	public:
		AtomicULong() { Value = 0; }

		AtomicULong(unsigned long InitialValue) { Value = InitialValue; }

		////////////////////////////////////////////////////////////
		//unsigned long SetVal( unsigned long NewValue )
		// lockless threadsafe set operation on the contained value
		//
		// Parameters
		//  unsigned long val  -  value to be set
		//
		// Return values
		//  Returns the initial value contained
		///////////////////////////////////////////////////////////
		unsigned long SetVal(unsigned long NewValue);


		///////////////////////////////////////////////////////////
		//unsigned long GetVal()
		// non-threadsafe get operation on the contained value
		//
		// Parameters
		//  None
		//
		// Return values
		//  Returns the value contained
		//////////////////////////////////////////////////////////
		unsigned long GetVal() { return Value; }


	private:
		// Disabled copy constructor
		AtomicULong(const AtomicULong & other) {}

		// Disabled assignment operator
		AtomicULong operator=(AtomicULong & other) { return *this; }


	protected:
		__declspec(align(4))  volatile unsigned long Value;
	};

	//////////////////////////////////////////////////////////
	//class AtomicCounter
	//  Derives from AtomicULong.
	//  Implements atomic incrementation and decrementation
	//
	//////////////////////////////////////////////////////////
	class AtomicCounter : public AtomicULong
	{
	public:
		AtomicCounter() { Value = 0; }
		AtomicCounter(unsigned long InitialValue) { Value = InitialValue; }


		//////////////////////////////////////////////////////////
		//unsigned long operator++()
		// lockless threadsafe prefix increment operator
		//
		// Parameters
		//  None
		//
		// Return values
		//  Returns the incremented value
		/////////////////////////////////////////////////////////
		unsigned long operator++();



		/////////////////////////////////////////////////////////
		//unsigned long operator--()
		// lockless threadsafe prefix decrement operator
		//
		// Parameters
		//  None
		//
		// Return values
		//  Returns the decremented value
		////////////////////////////////////////////////////////
		unsigned long operator--();



	private:
		// Disabled copy constructor
		AtomicCounter(const AtomicCounter & other) {}

		// Disabled assignment operator
		AtomicCounter operator=(const AtomicCounter & other) { return *this; }
	};

	//////////////////////////////////////////////////////
	//class AtomicBoolean
	//  Stores a Boolean atomically, using an AtomicULong
	//
	//////////////////////////////////////////////////////
	class AtomicBoolean
	{

	public:
		AtomicBoolean() : Value(0) {}

		AtomicBoolean(bool val)
		{
			if(val)
				Value.SetVal(1);
			else
				Value.SetVal(0);
		}

		////////////////////////////////////////////////////////////
		//bool SetVal( bool val )
		// lockless threadsafe set operation on the contained value
		//
		// Parameters
		//  bool val  -  value to be set
		//
		// Return values
		//  Returns the initial value contained
		///////////////////////////////////////////////////////////
		bool SetVal(bool val);


		///////////////////////////////////////////////////////////
		//bool GetVal()
		// non-threadsafe get operation on the contained value
		//
		// Parameters
		//  None
		//
		// Return values
		//  Returns the value contained
		//////////////////////////////////////////////////////////
		bool GetVal()
		{
			unsigned long val = 0;

			val = Value.GetVal();

			return (val & 1);
		}

	private:
		// Disabled copy constructor
		AtomicBoolean(const AtomicBoolean & other) {}

		// Disabled assignment operator
		AtomicBoolean operator=(const AtomicBoolean & other) { return *this; }

		AtomicULong Value;
	};

	////////////////////////////////////////////////
	//class AtomicFloat
	//  Stores a Float atomically.
	//  Implemented using AtomicULong.
	//
	////////////////////////////////////////////////
	class AtomicFloat
	{
	public:
		AtomicFloat() : Value(0) {}

		AtomicFloat(float InitialValue)
		{
			unsigned long iv = *(reinterpret_cast< unsigned long* >(&InitialValue));
			Value.SetVal(iv);
		}


		////////////////////////////////////////////////////////////
		//float SetVal( float NewValue )
		// lockless threadsafe set operation on the contained value
		//
		// Parameters
		//  float val  -  value to be set
		//
		// Return values
		//  Returns the initial value contained
		///////////////////////////////////////////////////////////
		float SetVal(float NewValue);


		///////////////////////////////////////////////////////////
		//bool GetVal()
		// non-threadsafe get operation on the contained value
		//
		// Parameters
		//  None
		//
		// Return values
		//  Returns the value contained
		//////////////////////////////////////////////////////////
		float GetVal();

	private:
		// Disabled copy constructor
		AtomicFloat(const AtomicFloat & other) {}

		// Disabled assignment operator
		AtomicFloat operator=(const AtomicFloat & other) { return *this; }

		AtomicULong Value;
	};
};

#endif // Atomic_h__