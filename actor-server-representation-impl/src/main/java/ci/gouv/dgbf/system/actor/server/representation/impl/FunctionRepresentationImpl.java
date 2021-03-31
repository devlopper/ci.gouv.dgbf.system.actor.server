package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.representation.api.FunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.FunctionDto;

@ApplicationScoped
public class FunctionRepresentationImpl extends AbstractRepresentationEntityImpl<FunctionDto> implements FunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response getExecutionHolders() {
		Runner.Arguments runnerArguments = new Runner.Arguments();
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runner.Arguments getRunnerArguments() {
				return runnerArguments;
			}
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						runnerArguments.setResult(FunctionQuerier.getInstance().readExecutionHolders(null));
					}
				};
			}
		});
	}

	@Override
	public Response getExecutionHoldersAndAssistants() {
		Runner.Arguments runnerArguments = new Runner.Arguments();
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runner.Arguments getRunnerArguments() {
				return runnerArguments;
			}
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						runnerArguments.setResult(FunctionQuerier.getInstance().readExecutionHoldersAndAssistants(null));
					}
				};
			}
		});
	}
	
}
