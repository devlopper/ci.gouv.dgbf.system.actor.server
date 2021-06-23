package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.rest.ResponseBuilder;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;
import org.cyk.utility.representation.server.AbstractSpecificRepresentationImpl.AbstractRunnableImpl;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;

@Path(ActorOpenAPIImpl.PATH)
@Tag(name = "Acteur")
public class ActorOpenAPIImpl extends AbstractOpenAPIImpl implements Serializable {

	public static final String PATH = "open/acteur";
	
	public static final String OPERATION_CREATE = "creer";
	@POST
	@Path(OPERATION_CREATE)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({ MediaType.TEXT_PLAIN})
	@Operation(description = "Créer un acteur",operationId = "creer_acteur")
	@APIResponses(value = {
			@APIResponse(description = "Acteur créé",responseCode = "200", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la création de l'acteur",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})	
	public Response create(
			@Parameter(allowEmptyValue = false,description = "Nom",example = "Komenan",name = "nom",required = true)
			@FormParam("nom") String firstName
			
			,@Parameter(allowEmptyValue = false,description = "Prénoms",example = "Yao Chrstian",name = "prenoms",required = true)
			@FormParam("prenoms") String lastNames
			
			,@Parameter(description = "Email",example = "komenanyc@yahoo.fr",name = "email",required = true)
			@FormParam("email") String electronicMailAddress
			
			,@Parameter(description = "Identifiant de la civilité",example = "01",name = "civilite")
			@FormParam("civilite") String civilityIdentifier
			
			,@Parameter(description = "Identifiant du groupe",example = "02",name = "groupe")
			@FormParam("groupe") String groupIdentifier) {
		
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						Actor actor = new Actor();
						actor.setFirstName(firstName).setLastNames(lastNames).setElectronicMailAddress(electronicMailAddress).setCivilityFromIdentifier(civilityIdentifier)
							.setGroupFromIdentifier(groupIdentifier).setEmailSendableAfterCreation(Boolean.TRUE);
						__inject__(ActorBusiness.class).create(actor);
						responseBuilderArguments.setStatus(Response.Status.CREATED);
						return new TransactionResult().incrementNumberOfCreation(1l).setTupleName("acteur");
					}
				};
			}
		});
	}
	
	public static final String OPERATION_GET = "obtenir";
	@GET
	@Path(OPERATION_GET)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les informations d'un acteur",operationId = "obtenir_acteur")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",description = "Informations de l'acteur obtenues", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(responseCode = "400",description = "Nom d'utilisateur obligatoire", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "404",description = "Nom d'utilisateur inconnu", content = @Content(mediaType = MediaType.TEXT_PLAIN))
	})	
	public Response get(
			@Parameter(allowEmptyValue = false,description = "Nom d'utilisateur",example = "komenan",name = PARAMETER_USER_NAME,required = true)
			@QueryParam(PARAMETER_USER_NAME) String code) {		
		if(StringHelper.isBlank(code))
			return Response.status(Status.BAD_REQUEST).entity("Nom d'utilisateur obligatoire").build();
		Actor actor = EntityReader.getInstance().readOne(Actor.class, new QueryExecutorArguments().setQuery(new Query()
				.setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC_ONE)).addFilterFieldsValues(ActorQuerier.PARAMETER_NAME_CODE,code));
		if(actor == null)
			return Response.status(Status.NOT_FOUND).entity("Nom d'utilisateur inconnu").build();
		return ResponseBuilder.getInstance().build(new ResponseBuilder.Arguments().setEntity(MappingHelper.getSource(actor, ActorDto.class)));
	}
	
	public static final String OPERATION_GET_ELECTRONIC_MAIL_ADDRESS = "creer";
	@GET
	@Path(OPERATION_GET_ELECTRONIC_MAIL_ADDRESS)
	@Produces({ MediaType.TEXT_PLAIN})
	@Operation(description = "Obtenir l'adresse électronique d'un acteur",operationId = "obtenir_adresse_electronique")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",description = "Adresse électronique de l'acteur", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "400",description = "Nom d'utilisateur obligatoire", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "404",description = "Nom d'utilisateur inconnu", content = @Content(mediaType = MediaType.TEXT_PLAIN))
	})
	public Response getElectronicMailAddress(
			@Parameter(allowEmptyValue = false,description = "Nom d'utilisateur",example = "komenan",name = PARAMETER_USER_NAME,required = true)
			@QueryParam(PARAMETER_USER_NAME) String code) {		
		if(StringHelper.isBlank(code))
			return Response.status(Status.BAD_REQUEST).entity("Nom d'utilisateur obligatoire").build();
		String electronicMailAddress = ActorQuerier.getInstance().readElectronicMailAddressByCode(code);
		if(StringHelper.isBlank(electronicMailAddress))
			return Response.status(Status.NOT_FOUND).entity("Nom d'utilisateur inconnu").build();
		return ResponseBuilder.getInstance().build(new ResponseBuilder.Arguments().setEntity(electronicMailAddress));
	}
	
	public static final String OPERATION_CHECK_EXISTENCE = "verifier_existence";
	@GET
	@Path(OPERATION_CHECK_EXISTENCE)
	@Produces({ MediaType.TEXT_PLAIN})
	@Operation(description = "Vérifier l'existence d'un acteur",operationId = "verifier_existence_acteur")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",description = "Acteur existe", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "400",description = "Nom d'utilisateur obligatoire", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "404",description = "Nom d'utilisateur inconnu", content = @Content(mediaType = MediaType.TEXT_PLAIN))
	})
	public Response checkExistense(
			@Parameter(allowEmptyValue = false,description = "Nom d'utilisateur",example = "komenan",name = PARAMETER_USER_NAME,required = true)
			@QueryParam(PARAMETER_USER_NAME) String code) {
		if(StringHelper.isBlank(code))
			return Response.status(Status.BAD_REQUEST).entity("Nom d'utilisateur obligatoire").build();
		if(!Boolean.TRUE.equals(CodeExecutor.getInstance().exists(Actor.class, code)))
			return Response.status(Status.NOT_FOUND).entity("Nom d'utilisateur inconnu").build();
		return Response.ok().build();
	}
	
	/**/
	
	public static final String PARAMETER_USER_NAME = "nom_utilisateur";
}