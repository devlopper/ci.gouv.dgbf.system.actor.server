package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.FunctionBusiness;
import ci.gouv.dgbf.system.actor.server.representation.api.FunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.FunctionDto;

@ApplicationScoped
public class FunctionRepresentationImpl extends AbstractRepresentationEntityImpl<FunctionDto> implements FunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response importFromKeycloak() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(FunctionBusiness.class).importFormKeycloak();
					}
				};
			}
		});
	}

	@Override
	public Response exportToKeycloak() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(FunctionBusiness.class).exportToKeycloak();
					}
				};
			}
		});
	}
	
}
