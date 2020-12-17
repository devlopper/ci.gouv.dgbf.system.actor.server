package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.configuration.ConfigurationHelper;
import org.cyk.utility.__kernel__.instance.InstanceCopier;
import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.variable.VariableName;
import org.cyk.utility.report.ReportRepresentation;
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
							setFromDto(requestDto, request);
						}
						__inject__(RequestBusiness.class).initialize(request);
					}
				};
			}
		});
	}
	
	@Override
	public Response record(RequestDto requestDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						Request database = EntityFinder.getInstance().find(Request.class,requestDto.getIdentifier());
						Request request = MappingHelper.getDestination(requestDto, Request.class);											
						InstanceCopier.getInstance().copy(database, request, List.of(Request.FIELD_TYPE,Request.FIELD_STATUS,Request.FIELD_CREATION_DATE
							,Request.FIELD_AUTHENTICATION_REQUIRED,Request.FIELD_ACCESS_TOKEN));
						setFromDto(requestDto, request);
						__inject__(RequestBusiness.class).record(request);
					}
				};
			}
		});
	}
	
	@Override
	public Response submitByIdentifier(String identifier) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).submitByIdentifier(identifier);
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
	
	@Override
	public Response buildReportByIdentifier(String identifier) {
		return __inject__(ReportRepresentation.class).get(ConfigurationHelper.getValueAsString(VARIABLE_NAME_REPORT_IDENTIFIER), null, null, null);
	}

	@Override
	public Response notifyAcessTokens(String electronicMailAddress) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).notifyAccessTokens(electronicMailAddress);
					}
				};
			}
		});
	}
	
	/**/
	
	private void setFromDto(RequestDto requestDto,Request request) {
		/*if(CollectionHelper.isNotEmpty(requestDto.getBudgetariesScopeFunctions())) {
			for(ScopeFunctionDto scopeFunctionDto : requestDto.getBudgetariesScopeFunctions()) {
				ScopeFunction scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, scopeFunctionDto.getIdentifier());
				if(scopeFunction == null)
					continue;
				if(request.getBudgetariesScopeFunctions() == null)
					request.setBudgetariesScopeFunctions(new ArrayList<>());
				request.getBudgetariesScopeFunctions().add(scopeFunction);
			}
		}*/
		if(requestDto.getActOfAppointmentSignatureDateAsTimestamp() == null)
			request.setActOfAppointmentSignatureDate(null);
		else
			request.setActOfAppointmentSignatureDate(LocalDate.ofInstant(Instant.ofEpochMilli(requestDto.getActOfAppointmentSignatureDateAsTimestamp()), ZoneId.systemDefault()));
	}

	/**/
	
	public static final String VARIABLE_NAME_REPORT_IDENTIFIER = VariableName.build("request.report.identifier");
}