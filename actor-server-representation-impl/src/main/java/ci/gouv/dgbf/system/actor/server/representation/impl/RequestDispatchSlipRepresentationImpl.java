package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.runnable.Runner.Arguments;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestDispatchSlipBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.representation.api.RequestDispatchSlipRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDispatchSlipDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;

@ApplicationScoped
public class RequestDispatchSlipRepresentationImpl extends AbstractRepresentationEntityImpl<RequestDispatchSlipDto> implements RequestDispatchSlipRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response record(RequestDispatchSlipDto requestDispatchSlipDto) {
		Arguments arguments = new Arguments();		
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Arguments getRunnerArguments() {
				return arguments;
			}
			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						RequestDispatchSlip requestDispatchSlip = StringHelper.isBlank(requestDispatchSlipDto.getIdentifier()) 
								? MappingHelper.getDestination(requestDispatchSlipDto, RequestDispatchSlip.class)
								: EntityFinder.getInstance().find(RequestDispatchSlip.class, requestDispatchSlipDto.getIdentifier());
						
						requestDispatchSlip.set__auditWho__(requestDispatchSlipDto.get__auditWho__());		
						
						if(StringHelper.isNotBlank(requestDispatchSlipDto.getFunctionIdentifier()))
							requestDispatchSlip.setFunction(EntityFinder.getInstance().find(Function.class, requestDispatchSlipDto.getFunctionIdentifier()));
						requestDispatchSlip.setRequests(null);
						if(CollectionHelper.isNotEmpty(requestDispatchSlipDto.getRequestsIdentifiers())) {
							for(String requestIdentifier : requestDispatchSlipDto.getRequestsIdentifiers()) {
								Request request = EntityFinder.getInstance().find(Request.class, requestIdentifier);
								if(request != null) {
									if(requestDispatchSlip.getRequests() == null)
										requestDispatchSlip.setRequests(new ArrayList<>());
									requestDispatchSlip.getRequests().add(request);
								}
							}
						}
						__inject__(RequestDispatchSlipBusiness.class).record(requestDispatchSlip);
						arguments.setResult(requestDispatchSlip);
					}
				};
			}
			
			@Override
			public ResponseBuilder getResponseBuilderWhenThrowableIsNull(Arguments runnerArguments) {
				ResponseBuilder responseBuilder = super.getResponseBuilderWhenThrowableIsNull(runnerArguments);
				if(getRunnerArguments() != null) {
					RequestDispatchSlip requestDispatchSlip = (RequestDispatchSlip) getRunnerArguments().getResult();
					if(requestDispatchSlip != null) {
						responseBuilder.header(RequestDispatchSlip.FIELD_IDENTIFIER, requestDispatchSlip.getIdentifier());
					}
				}
				return responseBuilder;
			}
		});
	}

	@Override
	public Response send(RequestDispatchSlipDto requestDispatchSlipDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						RequestDispatchSlip requestDispatchSlip = EntityFinder.getInstance().find(RequestDispatchSlip.class,requestDispatchSlipDto.getIdentifier());
						requestDispatchSlip.set__auditWho__(requestDispatchSlipDto.get__auditWho__());
						__inject__(RequestDispatchSlipBusiness.class).send(requestDispatchSlip);
					}
				};
			}
		});
	}
	
	@Override
	public Response process(RequestDispatchSlipDto requestDispatchSlipDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						RequestDispatchSlip requestDispatchSlip = EntityFinder.getInstance().find(RequestDispatchSlip.class,requestDispatchSlipDto.getIdentifier());
						requestDispatchSlip.set__auditWho__(requestDispatchSlipDto.get__auditWho__());
						if(CollectionHelper.isNotEmpty(requestDispatchSlipDto.getRequestsAcceptations())) {
							if(requestDispatchSlip.getRequests() == null)
								requestDispatchSlip.setRequests(new ArrayList<>());
							for(RequestDto.Acceptation acceptation : requestDispatchSlipDto.getRequestsAcceptations()) {
								Request request = EntityFinder.getInstance().find(Request.class,acceptation.getIdentifier());
								if(request == null)
									continue;
								requestDispatchSlip.getRequests().add(request.setStatusAsString(RequestStatus.CODE_ACCEPTED).setAcceptationComment(acceptation.getComment()));
							}						
						}
						if(CollectionHelper.isNotEmpty(requestDispatchSlipDto.getRequestsRejections())) {
							for(RequestDto.Rejection rejection : requestDispatchSlipDto.getRequestsRejections()) {
								Request request = EntityFinder.getInstance().find(Request.class,rejection.getIdentifier());
								if(request == null)
									continue;
								requestDispatchSlip.getRequests().add(request.setStatusAsString(RequestStatus.CODE_REJECTED).setRejectionReason(rejection.getReason()));
							}
						}
						__inject__(RequestDispatchSlipBusiness.class).process(requestDispatchSlip);
					}
				};
			}
		});
	}
}