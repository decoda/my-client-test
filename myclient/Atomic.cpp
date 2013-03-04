#include "StdAfx.h"
#include "Atomic.h"

namespace Threading
{

	unsigned long AtomicULong::SetVal(unsigned long NewValue)
	{
		unsigned long ret = 0;
		ret = InterlockedExchange(reinterpret_cast< volatile LONG* >(&Value), LONG(NewValue));
		return ret;
	}

	unsigned long AtomicCounter::operator++()
	{
		unsigned long val = 0;
		val = InterlockedIncrement(reinterpret_cast< volatile LONG* >(&Value));
		return val;
	}

	unsigned long AtomicCounter::operator--()
	{
		unsigned long val = 0;
		val = InterlockedDecrement(reinterpret_cast< volatile LONG* >(&Value));
		return val;
	}

	bool AtomicBoolean::SetVal(bool val)
	{
		unsigned long oldval = 0;

		if(val)
			oldval = Value.SetVal(1);
		else
			oldval = Value.SetVal(0);

		return (oldval & 1);
	}


	float AtomicFloat::SetVal(float NewValue)
	{
		unsigned long iv = 0;
		float ret = 0.0f;
		iv = *(reinterpret_cast< unsigned long* >(&NewValue));
		ret = *(reinterpret_cast< float* >(Value.SetVal(iv)));
		return ret;
	}


	float AtomicFloat::GetVal()
	{
		float val = 0.0f;
		val = *(reinterpret_cast< float* >(Value.GetVal()));
		return val;
	}
}