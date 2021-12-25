package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.configuration.ConfigurationHelper;
import org.cyk.utility.__kernel__.instance.InstanceCopier;
import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.__kernel__.runnable.Runner.Arguments;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.variable.VariableName;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.report.ReportRepresentation;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.representation.api.RequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;

@ApplicationScoped
public class RequestRepresentationImpl extends AbstractRepresentationEntityImpl<RequestDto> implements RequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response getPhotoByIdentifier(String identifier) {
		/*Runner.Arguments runnerArguments = new Runner.Arguments();
		runnerArguments.addRunnables(new Runnable() {					
			@Override
			public void run() {
				if(StringHelper.isBlank(identifier))
					throw new RuntimeException("Identifiant de la demande est obligatoire");						
				byte[] bytes = RequestQuerier.getInstance().readPhotoByIdentifier(identifier);
				if(bytes != null)
					runnerArguments.setResult(bytes);
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
		});*/
		if(StringHelper.isBlank(identifier))
			throw new RuntimeException("Identifiant de la demande est obligatoire");						
		return Response.ok(RequestQuerier.getInstance().readPhotoByIdentifier(identifier)).build();
	}
	
	@Override
	public Response getSignatureByIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			throw new RuntimeException("Identifiant de la demande est obligatoire");						
		return Response.ok(RequestQuerier.getInstance().readSignatureByIdentifier(identifier)).build();
	}
	
	@Override
	public Response getSignedRequestSheetByIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			throw new RuntimeException("Identifiant de la demande est obligatoire");						
		return Response.ok(RequestQuerier.getInstance().readSignedRequestSheetByIdentifier(identifier)).build();
	}
	
	@Override
	public Response getActOfAppointmentByIdentifier(String identifier) {
		/*Runner.Arguments runnerArguments = new Runner.Arguments();
		runnerArguments.addRunnables(new Runnable() {					
			@Override
			public void run() {
				if(StringHelper.isBlank(identifier))
					throw new RuntimeException("Identifiant de la demande est obligatoire");						
				byte[] bytes = RequestQuerier.getInstance().readSignatureByIdentifier(identifier);
				if(bytes != null)
					runnerArguments.setResult(bytes);
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
		*/
		if(StringHelper.isBlank(identifier))
			throw new RuntimeException("Identifiant de la demande est obligatoire");						
		return Response.ok(RequestQuerier.getInstance().readActOfAppointmentByIdentifier(identifier)).build();
	}
	
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
	public Response getOneToBeCreatedByTypeIdentifierByElectronicMailAddress(String typeIdentifier,String electronicMailAddress) {
		Runner.Arguments runnerArguments = new Runner.Arguments();
		runnerArguments.addRunnables(new Runnable() {					
			@Override
			public void run() {
				if(StringHelper.isBlank(typeIdentifier))
					throw new RuntimeException("Identifiant du type obligatoire");						
				Request request = __inject__(RequestBusiness.class).getOneToBeCreatedByTypeIdentifierByElectronicMailAddress(typeIdentifier, electronicMailAddress);
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
						Request request = MappingHelper.getDestination(requestDto, Request.class);
						if(request != null) {
							setFromDto(requestDto, request);
						}
						__inject__(RequestBusiness.class).initialize(request);
						arguments.setResult(request);
						responseBuilderArguments.setHeader(Request.FIELD_IDENTIFIER, request.getIdentifier());
					}
				};
			}
			
			/*@Override
			public ResponseBuilder getResponseBuilderWhenThrowableIsNull(Arguments runnerArguments) {
				ResponseBuilder responseBuilder = super.getResponseBuilderWhenThrowableIsNull(runnerArguments);
				if(getRunnerArguments() != null) {
					Request request = (Request) getRunnerArguments().getResult();
					if(request != null) {
						responseBuilder.header(Request.FIELD_IDENTIFIER, request.getIdentifier());
					}
				}
				return responseBuilder;
			}*/
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
							,Request.FIELD_AUTHENTICATION_REQUIRED,Request.FIELD_ACCESS_TOKEN
							//files
							,Request.FIELD_PHOTO,Request.FIELD_ACT_OF_APPOINTMENT,Request.FIELD_SIGNATURE,Request.FIELD_SIGNED_REQUEST_SHEET));
						setFromDto(requestDto, request);
						__inject__(RequestBusiness.class).record(request);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordPhoto(RequestDto requestDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						Request database = EntityFinder.getInstance().find(Request.class,requestDto.getIdentifier());
						//database.setPhoto(requestDto.get);
						__inject__(RequestBusiness.class).recordPhoto(database);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordPhotoByIdentifier(String identifier, byte[] bytes) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).recordPhotoByIdentifier(identifier, bytes);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordActOfAppointment(RequestDto requestDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						Request database = EntityFinder.getInstance().find(Request.class,requestDto.getIdentifier());
						//database.setPhoto(requestDto.get);
						__inject__(RequestBusiness.class).recordActOfAppointment(database);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordActOfAppointmentByIdentifier(String identifier, byte[] bytes) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).recordActOfAppointmentByIdentifier(identifier, bytes);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordSignature(RequestDto requestDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						Request database = EntityFinder.getInstance().find(Request.class,requestDto.getIdentifier());
						//database.setPhoto(requestDto.get);
						__inject__(RequestBusiness.class).recordSignature(database);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordSignatureByIdentifier(String identifier, byte[] bytes) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).recordSignatureByIdentifier(identifier, bytes);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordSignedRequestSheet(RequestDto requestDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						Request database = EntityFinder.getInstance().find(Request.class,requestDto.getIdentifier());
						//database.setPhoto(requestDto.get);
						__inject__(RequestBusiness.class).recordSignedRequestSheet(database);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordSignedRequestSheetByIdentifier(String identifier, byte[] bytes) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).recordSignedRequestSheetByIdentifier(identifier,null, bytes);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordSignedRequestSheetByIdentifierForAdmin(String identifier, byte[] bytes) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).recordSignedRequestSheetByIdentifier(identifier,Boolean.TRUE, bytes);
					}
				};
			}
		});
	}
	
	@Override
	public Response submitByIdentifier(String identifier,String readPageURL) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).submitByIdentifier(identifier,readPageURL);
					}
				};
			}
		});
	}
	
	@Override
	public Response returnByIdentifier(String identifier,String readPageURL) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).returnByIdentifier(identifier,readPageURL);
					}
				};
			}
		});
	}
	
	@Override
	public Response acceptByIdentifier(String identifier,List<String> budgetariesScopeFunctionsIdentifiers,String comment,String readPageURL,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).acceptByIdentifier(identifier,budgetariesScopeFunctionsIdentifiers,comment,readPageURL,actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response rejectByIdentifier(String identifier, String rejectionReason,String readPageURL,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).rejectByIdentifier(identifier, rejectionReason,readPageURL,actorCode);
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
	public Response notifyAcessTokens(String electronicMailAddress,String readPageURL) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).notifyAccessTokens(electronicMailAddress,readPageURL);
					}
				};
			}
		});
	}
	
	@Override
	public Response notifySignaturesSpecimens(String electronicMailAddress) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).notifySignaturesSpecimens(electronicMailAddress);
					}
				};
			}
		});
	}
	
	@Override
	public Response notifySignaturesSpecimensLink(String electronicMailAddress,String readPageURL) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).notifySignaturesSpecimensLink(electronicMailAddress,readPageURL);
					}
				};
			}
		});
	}
	
	@Override
	public Response recordSignatureSpecimenInformations(RequestDto requestDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						Request database = EntityFinder.getInstance().find(Request.class,requestDto.getIdentifier());
						Request request = MappingHelper.getDestination(requestDto, Request.class);
						InstanceCopier.getInstance().copy(database, request, List.of(Request.FIELD_TYPE,Request.FIELD_STATUS,Request.FIELD_CREATION_DATE
							,Request.FIELD_AUTHENTICATION_REQUIRED,Request.FIELD_ACCESS_TOKEN
							//files
							,Request.FIELD_PHOTO,Request.FIELD_ACT_OF_APPOINTMENT,Request.FIELD_SIGNATURE,Request.FIELD_SIGNED_REQUEST_SHEET));
						__inject__(RequestBusiness.class).recordSignatureSpecimenInformations(request);
					}
				};
			}
		});
	}
	
	@Override
	public Response exportForAccountCreation(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestBusiness.class).exportForAccountCreation(actorCode);
					}
				};
			}
		});
	}
	
	/**/
	
	private void setFromDto(RequestDto requestDto,Request request) {
		setBudgetariesScopeFunctionsFromIdentifiers(requestDto, request);
		setActOfAppointmentSignatureDateFromTimestamp(requestDto, request);
	}
	
	private void setBudgetariesScopeFunctionsFromIdentifiers(RequestDto requestDto,Request request) {
		request.setBudgetariesScopeFunctions(null);
		if(CollectionHelper.isNotEmpty(requestDto.getBudgetariesScopeFunctionsAsStrings())) {
			for(String identifier : requestDto.getBudgetariesScopeFunctionsAsStrings()) {
				ScopeFunction scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, identifier);
				if(scopeFunction == null)
					continue;
				if(request.getBudgetariesScopeFunctions() == null)
					request.setBudgetariesScopeFunctions(new ArrayList<>());
				request.getBudgetariesScopeFunctions().add(scopeFunction);
			}
		}
	}
	
	private void setActOfAppointmentSignatureDateFromTimestamp(RequestDto requestDto,Request request) {
		if(requestDto.getActOfAppointmentSignatureDateAsTimestamp() == null)
			request.setActOfAppointmentSignatureDate(null);
		else
			request.setActOfAppointmentSignatureDate(LocalDate.ofInstant(Instant.ofEpochMilli(requestDto.getActOfAppointmentSignatureDateAsTimestamp()), ZoneId.systemDefault()));
	}

	/**/
	
	// TODO each request type can have its own report identifier
	public static final String VARIABLE_NAME_REPORT_IDENTIFIER = VariableName.build("request.report.identifier");
}