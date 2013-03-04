#include "StdAfx.h"
#include "PlayerManager.h"
#include "Player.h"
#include "findpath.h"

initialiseSingleton(CPlayerManager);
CPlayerManager::CPlayerManager(void)
: m_PlayerAllocator(10)
{
	InitMapData();
}

CPlayerManager::~CPlayerManager(void)
{
}

void CPlayerManager::Draw(HDC hdc)
{
	static HPEN rhp = ::CreatePen(PS_SOLID, 1, RGB(50, 50, 50));
	HGDIOBJ Obj = SelectObject(hdc, rhp);
	for (int i = 0; i < 30; ++i)
	{
		int y = i * 20;
		::MoveToEx(hdc, 0, y, NULL);
		::LineTo(hdc, 800, y);
		for (int j = 0; j < 40; ++j)
		{
			int x = j * 20;
			::MoveToEx(hdc, x, 0, NULL);
			::LineTo(hdc, x, 600);
		}
	}
	SelectObject(hdc, Obj);

	static HBRUSH rhb = ::CreateSolidBrush(RGB(255, 0, 0));
	const char *val = mapdata;
	for (int i = 0; i < 30; ++i)
	{
		for (int j = 0; j < 40; ++j)
		{
			if (*val == 10)
			{
				int x1 = j * MAP_BLOCK_DISTANCE;
				int y1 = i * MAP_BLOCK_DISTANCE;
				int x2 = x1 + MAP_BLOCK_DISTANCE;
				int y2 = y1 + MAP_BLOCK_DISTANCE;
				RECT rt= {x1, y1, x2, y2};
				::FillRect(hdc, &rt, rhb);
			}
			val++;
		}
	}

	for (it_map_player it = m_Player.begin(); it != m_Player.end(); ++it)
	{
		Player *p = it->second;
		if (p != NULL)
			p->Draw(hdc);
	}
}

void CPlayerManager::Run(uint32 elapse)
{
	for (it_map_player it = m_Player.begin(); it != m_Player.end(); ++it)
	{
		Player *p = it->second;
		if (p != NULL)
			p->Run(elapse);
	}
}

void CPlayerManager::SetBlock(int x, int y)
{
	::SetBlock(x, y);
}


Player* CPlayerManager::GetPlayer(uint64 id)
{
	it_map_player it = m_Player.find(id);
	if (it != m_Player.end())
	{
		return it->second;
	}
	return NULL;
}


void CPlayerManager::AddPlayer(Player *p)
{
	if (p)
	{
		m_Player[p->GetID()] = p;
	}
}

void CPlayerManager::RemovePlayer(Player *p)
{
	if (p)
	{
		m_Player.erase(p->GetID());
	}
}

void CPlayerManager::OnMoveTo(uint64 id, int x, int y)
{
	it_map_player it = m_Player.find(id);
	if (it != m_Player.end())
	{
		Player *p = it->second;
		if (p != NULL)
			p->OnMsgMoveTo(x, y);
	}
}

void CPlayerManager::ResetPosition(uint64 id, int x, int y)
{
	it_map_player it = m_Player.find(id);
	if (it != m_Player.end())
	{
		Player *p = it->second;
		if (p != NULL)
			p->ResetPosition(x, y);
	}
}


