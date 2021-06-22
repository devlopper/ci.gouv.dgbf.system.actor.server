package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

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
import org.cyk.utility.representation.server.AbstractSpecificRepresentationImpl.AbstractRunnableImpl;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.servers.Server;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;

@Path(OpenAPIImpl.PATH)
@Server(url = "/api")
public class OpenAPIImpl extends AbstractOpenAPIImpl {

	public static final String PATH = "open";
	
	/* Acteur */
	private static final String TAG_ACTOR = "Acteur";
	private static final String PATH_ACTOR = "acteur";
	private static final String PATH_ACTOR_CREATE = PATH_ACTOR+"/creer";
	
	@POST
	@Path(PATH_ACTOR_CREATE)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({ MediaType.TEXT_PLAIN})
	@Tag(name = TAG_ACTOR)
	@Operation(description = "Créer un acteur",operationId = "creer_acteur")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",description = "Acteur créé", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "500",description = "Erreur lors de la création de l'acteur", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})	
	public Response createActor(
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
							.setGroupFromIdentifier(groupIdentifier);
						__inject__(ActorBusiness.class).create(actor);
						responseBuilderArguments.setStatus(Response.Status.CREATED);
						return new TransactionResult().incrementNumberOfCreation(1l).setTupleName("acteur");
					}
				};
			}
		});
	}
	
	private static final String PATH_ACTOR_GET = PATH_ACTOR+"/obtenir";
	
	@GET
	@Path(PATH_ACTOR_GET)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Tag(name = TAG_ACTOR)
	@Operation(description = "Obtenir un acteur",operationId = "obtenir_acteur")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",description = "Acteur obtenu", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(responseCode = "400",description = "Nom d'utilisateur obligatoire", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "404",description = "Nom d'utilisateur inconnu", content = @Content(mediaType = MediaType.TEXT_PLAIN))
	})	
	public Response getActor(
			@Parameter(allowEmptyValue = false,description = "Nom d'utilisateur",example = "komenan",name = "nom_utilisateur",required = true)
			@QueryParam("nom_utilisateur") String code) {		
		if(StringHelper.isBlank(code))
			return Response.status(Status.BAD_REQUEST).entity("Nom d'utilisateur obligatoire").build();
		Actor actor = EntityReader.getInstance().readOne(Actor.class, new QueryExecutorArguments().setQuery(new Query()
				.setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC_ONE)).addFilterFieldsValues(ActorQuerier.PARAMETER_NAME_CODE,code));
		if(actor == null)
			return Response.status(Status.NOT_FOUND).entity("Nom d'utilisateur inconnu").build();
		return ResponseBuilder.getInstance().build(new ResponseBuilder.Arguments().setEntity(MappingHelper.getSource(actor, ActorDto.class)));
	}
	
	private static final String PATH_ACTOR_EXISTS = PATH_ACTOR+"/exister";
	
	@GET
	@Path(PATH_ACTOR_EXISTS)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.TEXT_PLAIN})
	@Tag(name = TAG_ACTOR)
	@Operation(description = "Vérifier l'existence d'un acteur",operationId = "verifier_existence_acteur")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",description = "Existence de l'acteur vérifiée", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "400",description = "Nom d'utilisateur obligatoire", content = @Content(mediaType = MediaType.TEXT_PLAIN))
	})
	public Response isActorExists(
			@Parameter(allowEmptyValue = false,description = "Nom d'utilisateur",example = "komenan",name = "nom_utilisateur",required = true)
			@QueryParam("nom_utilisateur") String code) {
		if(StringHelper.isBlank(code))
			return Response.status(Status.BAD_REQUEST).entity("Nom d'utilisateur obligatoire").build();
		Actor actor = EntityReader.getInstance().readOne(Actor.class, new QueryExecutorArguments().setQuery(new Query()
				.setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC_ONE)).addFilterFieldsValues(ActorQuerier.PARAMETER_NAME_CODE,code));
		System.out.println("OpenAPIImpl.isActorExists() ::: "+actor);
		return ResponseBuilder.getInstance().build(new ResponseBuilder.Arguments().setEntity(actor == null ? Boolean.FALSE : Boolean.TRUE));
	}
}