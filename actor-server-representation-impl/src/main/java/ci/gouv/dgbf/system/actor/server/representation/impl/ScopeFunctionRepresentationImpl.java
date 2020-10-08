package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionDto;

@ApplicationScoped
public class ScopeFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeFunctionDto> implements ScopeFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response deriveAll() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).deriveAll();
					}
				};
			}
		});
	}
	
	@Override
	public Response codifyAll() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).codifyAll();
					}
				};
			}
		});
	}
}
