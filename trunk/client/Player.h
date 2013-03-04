#ifndef Player_h__
#define Player_h__

#include "WorldPacket.h"

class MySocket;

typedef std::list<Point2d> list_Point;

class Player
{
public:
	Player(uint64 id = 0);
	~Player();

	static void* operator new(std::size_t);
	static void operator delete(void*, std::size_t);

public:
	virtual void Run(uint32 elapse);
	virtual bool MoveTo(int x, int y);
	virtual void OnMoveTo(int x, int y){};
	virtual void SetSelect(bool on){ m_bSelected = on; }
	void Draw(HDC hdc);
	void SetID(uint64 id) { m_ID = id; }
	uint64 GetID() const { return m_ID; }
	bool GetSelect() const { return m_bSelected; }
	void ResetPosition(int x, int y);
	void OnMsgMoveTo(int x, int y);
	int GetSceneX() const { return m_nSceneX; }
	int GetSceneY() const { return m_nSceneY; }
	Point2d GetLocation() const { return Point2d(m_nX, m_nY); }
	Point2d GetSceneLocation() const { return Point2d(m_nSceneX, m_nSceneY); }
	void SetDirection(int direction) { m_nDirection = direction; }
	bool IsMoving() const { return m_bMoving; }

private:
	void PrepareStep();
	int CalculateStepDirection(Point2d step);
	Point2d CalculateIncrement(uint32 elapse);
	bool IsStepOver();
	void RevisePlayerSceneLocation();

protected:
	uint64 m_ID;
	float m_fSpeed;	// ËÙ¶È
	int m_nX;
	int m_nY;
	int m_nSceneX;
	int m_nSceneY;
	int m_nDirection;
	Point2d	m_ptNextStep;
	Point2d m_ptDes;
	list_Point m_lsPath;
	bool m_bSelected;
	bool m_bMoving;
};

#endif // Player_h__