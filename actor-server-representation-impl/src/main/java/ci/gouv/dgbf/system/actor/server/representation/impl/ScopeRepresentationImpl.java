package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.map.CollectionOfMapsStringStringBuilder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.representation.Arguments;
import org.cyk.utility.representation.EntityReader;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeDto;

@ApplicationScoped
public class ScopeRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeDto> implements ScopeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response getSectionsByActorCode(String actorCode) {
		return __getByActorCode__(actorCode,ScopeOfTypeSectionQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}
	
	@Override
	public Response getBudgetSpecializationUnitsByActorCode(String actorCode) {
		return __getByActorCode__(actorCode,ScopeOfTypeBudgetSpecializationUnitQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}
	
	@Override
	public Response getActionsByActorCode(String actorCode) {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public Response getActivitiesByActorCode(String actorCode) {
		return __getByActorCode__(actorCode,ScopeOfTypeActivityQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}
	
	@Override
	public Response getLinesByActorCode(String actorCode) {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public Response getAdministrativeUnitsByActorCode(String actorCode) {
		return __getByActorCode__(actorCode,ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}
	
	/**/
	
	private Response __getByActorCode__(String actorCode,String queryIdentifier) {
		if(StringHelper.isBlank(actorCode))
			return Response.status(Status.BAD_REQUEST).entity("nom d'utilisateur obligatoire").build();
		Actor actor = __inject__(ActorPersistence.class).readByBusinessIdentifier(actorCode);
		if(actor == null)
			return Response.status(Status.BAD_REQUEST).entity("nom d'utilisateur inconnu").build();
		Arguments arguments = new Arguments().setQueryExecutorArguments(new QueryExecutorArguments.Dto());
		arguments.setRepresentationEntityClass(ScopeDto.class);
		arguments.getQueryExecutorArguments().setQueryIdentifier(queryIdentifier).addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode);
		arguments.setListener(new Arguments.Listener.AbstractImpl<ScopeDto,Scope>() {
			@Override
			public void processPersistenceEntities(Collection<Scope> persistenceEntities) {
				super.processPersistenceEntities(persistenceEntities);
				if(CollectionHelper.isEmpty(persistenceEntities))
					return;
				CollectionHelper.cast(Scope.class, persistenceEntities).forEach(scope -> {
					scope.setIdentifier(null);
					scope.setType(null);
				});
			}
		});
		return EntityReader.getInstance().read(arguments);
	}
	
	/**/
	
	public static Response get(String typeCode,String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples
			,Boolean removeTypeCodeFromIdentifier) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<Scope> scopes = __inject__(ScopeBusiness.class).get(typeCode, actorCode, visible, pageable, firstTupleIndex, numberOfTuples
								,removeTypeCodeFromIdentifier);
						CollectionOfMapsStringStringBuilder.Arguments<Scope> arguments = new CollectionOfMapsStringStringBuilder.Arguments<Scope>()
								.setCollection(scopes).addFieldsNames(Scope.FIELD_IDENTIFIER,ScopeDto.JSON_FIELD_IDENTIFIER
										,Scope.FIELD_CODE,ScopeDto.JSON_FIELD_CODE,Scope.FIELD_NAME,ScopeDto.JSON_FIELD_NAME);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(Scope.class, arguments));
					}
				};
			}
		});
	}
	
	public static Response getByActorCode(String typeCode,String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples
			,Boolean removeTypeCodeFromIdentifier) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<Scope> scopes = __inject__(ScopeBusiness.class).getByActorCode(actorCode,typeCode, visible, pageable, firstTupleIndex, numberOfTuples
								,removeTypeCodeFromIdentifier);
						CollectionOfMapsStringStringBuilder.Arguments<Scope> arguments = new CollectionOfMapsStringStringBuilder.Arguments<Scope>()
								.setCollection(scopes).addFieldsNames(Scope.FIELD_IDENTIFIER,ScopeDto.JSON_FIELD_IDENTIFIER
										,Scope.FIELD_CODE,ScopeDto.JSON_FIELD_CODE,Scope.FIELD_NAME,ScopeDto.JSON_FIELD_NAME);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(Scope.class, arguments));
					}
				};
			}
		});
	}
	
	public static Response getByTypeCodeByActorCode(String typeCode,String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples
			,Boolean removeTypeCodeFromIdentifier) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<Scope> scopes = __inject__(ScopeBusiness.class).getByTypeCodeByActorCode(typeCode, actorCode, visible, pageable, firstTupleIndex, numberOfTuples
								,removeTypeCodeFromIdentifier);
						CollectionOfMapsStringStringBuilder.Arguments<Scope> arguments = new CollectionOfMapsStringStringBuilder.Arguments<Scope>()
								.setCollection(scopes).addFieldsNames(Scope.FIELD_IDENTIFIER,ScopeDto.JSON_FIELD_IDENTIFIER
										,Scope.FIELD_CODE,ScopeDto.JSON_FIELD_CODE,Scope.FIELD_NAME,ScopeDto.JSON_FIELD_NAME);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(Scope.class, arguments));
					}
				};
			}
		});
	}
	
	public static Response getAdministrativeUnitsByActorCode(String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples
			,Boolean removeTypeCodeFromIdentifier) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<Scope> scopes = __inject__(ScopeBusiness.class).getAdministrativeUnitsByActorCode(actorCode, visible, pageable, firstTupleIndex, numberOfTuples
								,removeTypeCodeFromIdentifier);
						CollectionOfMapsStringStringBuilder.Arguments<Scope> arguments = new CollectionOfMapsStringStringBuilder.Arguments<Scope>()
								.setCollection(scopes).addFieldsNames(Scope.FIELD_IDENTIFIER,ScopeDto.JSON_FIELD_IDENTIFIER
										,Scope.FIELD_CODE,ScopeDto.JSON_FIELD_CODE,Scope.FIELD_NAME,ScopeDto.JSON_FIELD_NAME,Scope.FIELD_SECTION_AS_STRING
										,ScopeDto.JSON_FIELD_SECTION_IDENTIFIER);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(Scope.class, arguments));
					}
				};
			}
		});
	}
	
	public static Response getBudgetSpecializationUnitsByActorCode(String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples
			,Boolean removeTypeCodeFromIdentifier) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<Scope> scopes = __inject__(ScopeBusiness.class).getBudgetSpecializationUnitsByActorCode(actorCode, visible, pageable, firstTupleIndex, numberOfTuples
								,removeTypeCodeFromIdentifier);
						CollectionOfMapsStringStringBuilder.Arguments<Scope> arguments = new CollectionOfMapsStringStringBuilder.Arguments<Scope>()
								.setCollection(scopes).addFieldsNames(Scope.FIELD_IDENTIFIER,ScopeDto.JSON_FIELD_IDENTIFIER
										,Scope.FIELD_CODE,ScopeDto.JSON_FIELD_CODE,Scope.FIELD_NAME,ScopeDto.JSON_FIELD_NAME,Scope.FIELD_SECTION_AS_STRING
										,ScopeDto.JSON_FIELD_SECTION_IDENTIFIER);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(Scope.class, arguments));
					}
				};
			}
		});
	}
	
	public static Response getVisibleSections(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						responseBuilderArguments.setEntity(__inject__(ScopeBusiness.class).getVisibleSections(actorCode));
					}
				};
			}
		});
	}
	
	public static Response getVisibleAdministrativeUnits(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						responseBuilderArguments.setEntity(__inject__(ScopeBusiness.class).getVisibleAdministrativeUnits(actorCode));
					}
				};
			}
		});
	}
	
	public static Response getVisibleBudgetSpecializationUnits(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						responseBuilderArguments.setEntity(__inject__(ScopeBusiness.class).getVisibleBudgetSpecializationUnits(actorCode));
					}
				};
			}
		});
	}
}