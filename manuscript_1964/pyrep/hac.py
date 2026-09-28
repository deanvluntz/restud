import numpy as np, pandas as pd, statsmodels.api as sm
def vcovHAC(X, e):
    X=np.asarray(X,float); e=np.asarray(e,float); n,k=X.shape
    U=X*e[:,None]
    w=np.array([0.0 if np.allclose(X[:,j],1) else 1.0 for j in range(k)])
    num=den=0.0
    for j in range(k):
        u=U[:,j]
        rho=np.dot(u[1:],u[:-1])/np.dot(u[:-1],u[:-1])
        res=u[1:]-rho*u[:-1]; s2=np.dot(res,res)/(n-1)
        num+=w[j]*4*rho**2*s2**2/((1-rho)**8)
        den+=w[j]*s2**2/((1-rho)**4)
    a2=num/den; bw=1.3221*(a2*n)**0.2
    def qs(x):
        if x==0: return 1.0
        z=6*np.pi*x/5
        return 25/(12*np.pi**2*x**2)*(np.sin(z)/z-np.cos(z))
    meat=U.T@U/n
    for lag in range(1,n):
        wl=qs(lag/bw); G=U[lag:].T@U[:-lag]/n; meat+=wl*(G+G.T)
    meat*=n/(n-k)
    bread=np.linalg.inv(X.T@X/n)
    return bread@meat@bread/n
