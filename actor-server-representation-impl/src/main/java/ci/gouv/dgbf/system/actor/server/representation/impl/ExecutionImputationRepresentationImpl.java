package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.representation.api.ExecutionImputationRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutionImputationDto;

@ApplicationScoped
public class ExecutionImputationRepresentationImpl extends AbstractRepresentationEntityImpl<ExecutionImputationDto> implements ExecutionImputationRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response refreshMaterializedView() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ExecutionImputationQuerier.refreshMaterializedView();
					}
				};
			}
		});
	}
}