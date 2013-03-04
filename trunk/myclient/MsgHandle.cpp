#include "StdAfx.h"
#include "MainPlayer.h"
#include "PlayerManager.h"

void MainPlayer::HandleLogin(WorldPacket &pack)
{
	uint64 id = 0;
	uint32 x = 0, y = 0;
	pack >> id >> x >> y;
	SetID(id);
	ResetPosition(x, y);
	printf("player login %i\n", m_ID);
	WorldPacket mapinfo(10003);
	SendPacket(mapinfo);
}

void MainPlayer::HandlePath(WorldPacket &pack)
{
	uint64 id = 0;
	pack >> id;
	if (id == m_ID)
	{
		--m_nStepError;
	}
	else
	{
		uint32 x = 0, y = 0;
		pack >> x >> y;
		sPlayerMgr.OnMoveTo(id, x, y);
	}
}

void MainPlayer::HandleAddObj(WorldPacket &pack)
{
	uint16 len = 0;
	pack >> len;
	for (int i = 0; i < len; ++i)
	{
		uint64 id = 0;
		uint32 x = 0, y = 0;
		pack >> id >> x >> y;
		Player *p = sPlayerMgr.GetPlayer(id);
		if (p)
		{
			ASSERT(0);
		}
		else
		{
			p = new Player();
		}
		if (p)
		{
			p->SetID(id);
			p->ResetPosition(x, y);
			sPlayerMgr.AddPlayer(p);
		}
	}
}

void MainPlayer::HandleRemoveObj(WorldPacket &pack)
{
	uint16 len = 0;
	pack >> len;
	for (int i = 0; i < len; ++i)
	{
		uint64 id = 0;
		pack >> id;
		Player *p = sPlayerMgr.GetPlayer(id);
		if (p)
		{
			sPlayerMgr.RemovePlayer(p);
			delete p;
		}
	}
}