package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeFunctionExecutionImputationRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionExecutionImputationDto;

@ApplicationScoped
public class ScopeFunctionExecutionImputationRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeFunctionExecutionImputationDto> implements ScopeFunctionExecutionImputationRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response deriveAll() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
					}
				};
			}
		});
	}
	
	@Override
	public Response deleteAll() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
					}
				};
			}
		});
	}	
}