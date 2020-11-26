package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.representation.api.RequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;

@ApplicationScoped
public class RequestRepresentationImpl extends AbstractRepresentationEntityImpl<RequestDto> implements RequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response getOneToBeCreatedByTypeIdentifier(String typeIdentifier) {
		Runner.Arguments runnerArguments = new Runner.Arguments();
		runnerArguments.addRunnables(new Runnable() {					
			@Override
			public void run() {
				if(StringHelper.isBlank(typeIdentifier))
					throw new RuntimeException("Identifiant du type obligatoire");						
				Request request = RequestQuerier.getInstance().instantiateOneByTypeIdentifier(typeIdentifier);
				if(request != null)
					runnerArguments.setResult(MappingHelper.getSource(request, RequestDto.class));
			}
		});
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return null;
			}
			@Override
			public Runner.Arguments getRunnerArguments() {
				return runnerArguments;
			}
		});
	}	
	
	@Override
	public Response initialize(RequestDto requestDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						Request request = MappingHelper.getDestination(requestDto, Request.class);
						if(request != null) {
							if(requestDto.getActOfAppointmentSignatureDateAsTimestamp() == null)
								request.setActOfAppointmentSignatureDate(null);
							else
								request.setActOfAppointmentSignatureDate(LocalDate.ofInstant(Instant.ofEpochMilli(requestDto.getActOfAppointmentSignatureDateAsTimestamp())
										, ZoneId.systemDefault()));
						}						
						__inject__(RequestBusiness.class).initialize(request);
					}
				};
			}
		});
	}
	
	@Override
	public Response acceptByIdentifier(String identifier) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).acceptByIdentifier(identifier);
					}
				};
			}
		});
	}
	
	@Override
	public Response rejectByIdentifier(String identifier, String rejectionReason) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).rejectByIdentifier(identifier, rejectionReason);
					}
				};
			}
		});
	}
}