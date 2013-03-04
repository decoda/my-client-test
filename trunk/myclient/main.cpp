#include "StdAfx.h"
#include "client.h"

int main(int argc, char* argv[])
{
	HINSTANCE hInstance = (HINSTANCE)GetModuleHandle(NULL);
	HINSTANCE hPreInstance = NULL;
	TCHAR szCmdLine[1024];
	szCmdLine[0] = 0;
	LPTSTR lpCmdLine = szCmdLine;
	for ( int i = 1; i < argc; i++ )
	{
		if ( i > 1 )
		{
			strncpy(lpCmdLine, " ", 1);
			lpCmdLine = lpCmdLine + strlen(" ");
		}
		strncpy(lpCmdLine, argv[i], strlen(argv[i]));
		lpCmdLine = lpCmdLine + strlen(argv[i]);
	}
	lpCmdLine = szCmdLine;
	int nCmdShow = SW_SHOWNORMAL;


	int ret = WinMain(hInstance, hPreInstance, lpCmdLine, nCmdShow);
	return 0;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow)
{
	// 建窗口
	new Client(hInstance);

	// 定制、创建、显示窗口
	sClient.Initialize(iCmdShow);

	//消息循环
	return sClient.Run();
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	return sClient.HandleEvent(hwnd, msg, wParam, lParam);
}