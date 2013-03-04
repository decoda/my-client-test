#include "StdAfx.h"
#include "Player.h"
#include "findpath.h"
#include "MySocket.h"
#include "PlayerManager.h"

Player::Player(uint64 id)
:m_nX(10)
,m_nY(10)
,m_nSceneX(0)
,m_nSceneY(0)
,m_fSpeed(0.15f)
,m_ID(id)
,m_bSelected(false)
,m_bMoving(false)
{
}

Player::~Player()
{

}

void* Player::operator new(std::size_t)
{
	return sPlayerMgr.NewPlayer();
}

void Player::operator delete(void* p, std::size_t)
{
	sPlayerMgr.FreePlayer((Player*)p);
}

void Player::Draw(HDC hdc)
{
	static HBRUSH bhb = ::CreateSolidBrush(RGB(0, 0, 255));
	static HBRUSH ghb = ::CreateSolidBrush(RGB(0, 255, 0));
	static HBRUSH greyhb = ::CreateSolidBrush(RGB(100, 100, 100));

// 	for (list_Point::iterator it = m_lsPath.begin(); it != m_lsPath.end(); ++it)
// 	{
// 		int x1 = it->x - 5;
// 		int y1 = it->y - 5;
// 		int x2 = it->x + 5;
// 		int y2 = it->y + 5;
// 		RECT rt= {x1, y1, x2, y2};
// 		::FillRect(hdc, &rt, ghb);
// 	}

	int x1 = m_nX - 5;
	int y1 = m_nY - 5;
	int x2 = m_nX + 5;
	int y2 = m_nY + 5;
	RECT rt= {x1, y1, x2, y2};
	::FillRect(hdc, &rt, m_bSelected ? bhb : greyhb);
}

void Player::Run(uint32 elapse)
{
	if (IsMoving())
	{
		if (IsStepOver())
		{
			PrepareStep();
		}
		else
		{
			Point2d dis = CalculateIncrement(elapse);
			if (dis.x != 0 || dis.y != 0)
			{
				m_nX += dis.x;
				m_nY += dis.y;
				RevisePlayerSceneLocation();
			}
		}
	}
}

void Player::OnMsgMoveTo(int x, int y)
{
	Point2d pt;
	pt.x = x;
	pt.y = y;
	m_lsPath.push_back(pt);
}

bool Player::MoveTo(int x, int y)
{
	m_ptDes = Point2d(x, y);
	Point2d start = GetSceneLocation();
	Point2d end = Point2d(x/MAP_BLOCK_DISTANCE, y/MAP_BLOCK_DISTANCE);
	m_lsPath.clear();
	if (FindPath(start, end, m_lsPath))
	{
		PrepareStep();
		return true;
	}
	else
	{
		printf("can not find path <%02d,%02d> to <%02d,%02d>\n", start.x, start.y, end.x, end.y);
		return false;
	}
}

void Player::ResetPosition(int x, int y)
{
	m_lsPath.clear();
}

void Player::PrepareStep()
{
	if (!m_lsPath.empty())
	{
		m_ptNextStep = m_lsPath.front();
		m_lsPath.pop_front();
		int dir = CalculateStepDirection(m_ptNextStep);
		if (dir != -1)
		{
			SetDirection(dir);
		}
		m_bMoving = true;
	}
	else
	{
		m_bMoving = false;
	}
}

int Player::CalculateStepDirection(Point2d step)
{
	int dx = step.x - m_nSceneX;
	int dy = step.y - m_nSceneY;
	int dir = 0;
	if (dx < 0) {
		if (dy < 0) {
			dir = DIRECTION_BOTTOM_LEFT;
		} else if (dy > 0) {
			dir = DIRECTION_TOP_LEFT;
		} else {
			dir = DIRECTION_LEFT;
		}
	} else if (dx > 0) {
		if (dy < 0) {
			dir = DIRECTION_BOTTOM_RIGHT;
		} else if (dy > 0) {
			dir = DIRECTION_TOP_RIGHT;
		} else {
			dir = DIRECTION_RIGHT;
		}
	} else {// x=0
		if (dy < 0) {
			dir = DIRECTION_BOTTOM;
		} else if (dy > 0) {
			dir = DIRECTION_TOP;
		} else {
			// no move
			dir = -1;
		}
	}
	return dir;
}

Point2d Player::CalculateIncrement(uint32 elapse)
{
	int dx = 0, dy = 0;
	// 如果该坐标可以到达移动
	if (true)
	{
		// 计算起点与目标点的弧度角
		double radian = ::atan(1.0 * (m_ptNextStep.y - m_nSceneY) / (m_ptNextStep.x - m_nSceneX));
		// 计算移动量
		int distance = (int) (m_fSpeed * elapse);
		if (distance > MAP_BLOCK_DISTANCE) {
			distance = MAP_BLOCK_DISTANCE;
		}
		dx = (int) (distance * ::cos(radian));
		dy = (int) (distance * ::sin(radian));
		// 修正移动方向
		if (m_ptNextStep.x > m_nSceneX) {
			dx = ::abs(dx);
		} else {
			dx = -::abs(dx);
		}
		if (m_ptNextStep.y > m_nSceneY) {
			dy = ::abs(dy);
		} else {
			dy = -::abs(dy);
		}
	}
	return Point2d(dx, dy);
}

// 是否走到下一步
bool Player::IsStepOver()
{
	Point2d pt = LocalToScene(GetLocation());
	return pt.x == m_ptNextStep.x && pt.y == m_ptNextStep.y;
}

void Player::RevisePlayerSceneLocation()
{
	Point2d pt =LocalToScene(GetLocation());
	m_nSceneX = pt.x;
	m_nSceneY = pt.y;
}
