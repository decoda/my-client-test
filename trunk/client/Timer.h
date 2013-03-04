#ifndef Timer_h__
#define Timer_h__

class CTimer : public Singleton<CTimer>
{
public:
	CTimer();
	~CTimer();

public:
	void Run();
	uint32 GetNow() const { return m_TimeNow; }
	uint32 GetStart() const { return m_TimeStart; }

private:
	uint32 GetTick();

private:
	uint32 m_TimeStart;
	uint32 m_TimeNow;
};

#define sTimer CTimer::getSingleton()

class CTimerThread : public CThread
{
public:
	bool run();
};

#endif // Timer_h__