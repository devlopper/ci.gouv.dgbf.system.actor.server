package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.representation.DataTransferObjectProcessor;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __initialize__(Object object) {
		__inject__(ci.gouv.dgbf.system.actor.server.business.impl.ApplicationScopeLifeCycleListener.class).initialize(null);
		__setQualifierClassTo__(ci.gouv.dgbf.system.actor.server.annotation.System.class, org.cyk.utility.server.representation.DataLoader.class
				,DataTransferObjectProcessor.class);
	}
	 
	@Override
	public void __destroy__(Object object) {}
	
}