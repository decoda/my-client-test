#include "StdAfx.h"
#include "client.h"
#include "PlayerManager.h"
#include "Player.h"
#include "MainPlayer.h"

LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam);

initialiseSingleton(Client);
Client::Client(HINSTANCE hInstance)
{
	m_hInstance = hInstance;
	m_nMouseX = m_nMouseY = 0;
}

Client::~Client()
{

}

void Client::Initialize(int nCmdShow)
{
	static TCHAR className[] = "client";
	static TCHAR windowName[] = "client";
	WNDCLASSEX winclass;

	winclass.cbSize = sizeof(WNDCLASSEX);
	winclass.style = CS_HREDRAW|CS_VREDRAW|CS_DBLCLKS;
	winclass.lpfnWndProc = WndProc;
	winclass.cbClsExtra = 0;
	winclass.cbWndExtra = 0;
	winclass.hInstance = m_hInstance;
	winclass.hIcon = LoadIcon (NULL, IDI_APPLICATION);
	winclass.hCursor = LoadCursor(NULL,IDC_ARROW);
	winclass.hbrBackground = (HBRUSH)CreateSolidBrush(RGB(16,16,30));
	winclass.lpszMenuName = NULL;
	winclass.lpszClassName = className;
	winclass.hIconSm = winclass.hIcon;
	RegisterClassEx(&winclass);
	m_hwnd = CreateWindow(className,
		windowName,
		WS_POPUPWINDOW|WS_CAPTION|WS_MINIMIZEBOX,
		100,100,
		800,600,
		NULL,
		NULL,
		m_hInstance,
		NULL);//创建窗口
	ShowWindow(m_hwnd,nCmdShow);
	UpdateWindow(m_hwnd);
}

int Client::Run(){	
	//消息循环
	MSG	msg;
	uint32 timeLastUpdate = sTimer.GetStart();
	while(true) 
	{
		if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) 
		{
			//处理消息
			if (msg.message == WM_QUIT) break;
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
		else
		{
			//处理游戏逻辑
			uint32 now = sTimer.GetNow();
			uint32 timeElapse = now - timeLastUpdate;
			if(timeElapse > 30) 
			{
				sClient.Update(timeElapse);
				timeLastUpdate = now;
			}
			else
			{
				::Sleep(1);
			}
		}
	}
	UnLoadContent();
	return msg.wParam ;
}

LRESULT Client::HandleEvent(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch (msg) 
	{
	case WM_SOCKET:
		sPlayerMain.OnSocketEvent(wParam, WSAGETSELECTERROR(lParam), WSAGETSELECTEVENT(lParam));
		return 0;
	case WM_CREATE:
		LoadContent();
		return 0;

	case WM_KEYDOWN:
		KeyDown(wParam);
		return 0;

	case WM_PAINT:	
		Draw(hwnd);
		return 0;

	case WM_LBUTTONDBLCLK:
		MouseLDButtonDown(lParam);
		return 0;

	case WM_MBUTTONDOWN:
		MouseMButtonDown(lParam);
		return 0;

	case WM_LBUTTONDOWN:
		MouseLButtonDown(lParam);
		return 0;

	case WM_RBUTTONDOWN:
		MouseRButtonDown(lParam);
		return 0;

	case WM_MOUSEMOVE:
		MouseMove(lParam);
		return 0;

	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
	}
	return DefWindowProc(hwnd, msg, wParam, lParam);
}

void Client::LoadContent()
{
	ThreadPool.Startup();
	
	// 定时器
	new CTimer();
	// 角色管理
	new CPlayerManager();
	// main
	new MainPlayer();

	ThreadPool.ExecuteTask(new CTimerThread());

	sPlayerMgr.AddPlayer(sPlayerMain.getSingletonPtr());
}

void Client::UnLoadContent()
{
	ThreadPool.Shutdown();

	delete MainPlayer::getSingletonPtr();
	delete CPlayerManager::getSingletonPtr();
	delete CTimer::getSingletonPtr();
}

void Client::Update(uint32 elapse) 
{	
	sPlayerMgr.Run(elapse);
	InvalidateRect(m_hwnd, NULL, FALSE);
}

void Client::Draw(HWND hwnd)
{
	PAINTSTRUCT ps;
	HDC hdc=BeginPaint(hwnd, &ps);
	HDC hdcMem = CreateCompatibleDC(hdc);
	HBITMAP hBmp=CreateCompatibleBitmap(hdc, 800, 600);
	HGDIOBJ obj = SelectObject(hdcMem,hBmp);

	::SetBkMode(hdcMem, TRANSPARENT);
	sPlayerMgr.Draw(hdcMem);

	COLORREF crOld = SetTextColor(hdcMem, RGB(150,150,150));
	char szTxt[32];
	sprintf(szTxt, "<%2d, %2d>", m_nMouseX, m_nMouseY);
	::TextOutA(hdcMem, 730, 0, szTxt, strlen(szTxt));
	SetTextColor(hdcMem, crOld);

	BitBlt(hdc,0,0,800,600,hdcMem,0,0,SRCCOPY); 
	SelectObject(hdcMem, obj);
	DeleteObject(hBmp);
	DeleteDC(hdcMem);
	EndPaint(hwnd, &ps);
}

void Client::MouseMove(LPARAM lParam)
{
	int x = LOWORD(lParam) / 20;
	int y = HIWORD(lParam) / 20;
	m_nMouseX = x;
	m_nMouseY = y;
}

void Client::MouseLButtonDown(LPARAM lParam)
{
	sPlayerMain.MoveTo(LOWORD(lParam), HIWORD(lParam));
}

void Client::MouseMButtonDown(LPARAM lParam)
{
	int x = LOWORD(lParam) / 20;
	int y = HIWORD(lParam) / 20;
	sPlayerMain.ResetPosition(x, y);
}

void Client::MouseLDButtonDown(LPARAM lParam)
{
	int x = LOWORD(lParam) / 20;
	int y = HIWORD(lParam) / 20;
	sPlayerMain.SetSelect(x, y);
}

void Client::MouseRButtonDown(LPARAM lParam)
{
	int x = LOWORD(lParam) / 20;
	int y = HIWORD(lParam) / 20;
	sPlayerMgr.SetBlock(x, y);
}

void Client::KeyDown(LPARAM wParam)
{
	switch (wParam)
	{
	case 'L':
		{
			const char szServer[] = "127.0.0.1";
			uint32 nPort = 55000;
			sPlayerMain.LoginServer(szServer, nPort);
		}
		return;
	default:break;
	}
}
