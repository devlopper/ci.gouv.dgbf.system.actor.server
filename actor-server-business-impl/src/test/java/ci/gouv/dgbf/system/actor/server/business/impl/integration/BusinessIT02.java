package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.variable.VariableHelper;
import org.cyk.utility.__kernel__.variable.VariableName;
import org.cyk.utility.business.server.EntityCreator;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.test.business.server.AbstractUnitTest;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;
import org.junit.Test;
import org.junit.runner.RunWith;

import ci.gouv.dgbf.system.actor.server.business.api.AccountRequestBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfileBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfilePrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.RejectedAccountRequestBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.business.impl.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.api.AccountRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.FunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentityPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.PrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.RejectedAccountRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationForm;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;
import ci.gouv.dgbf.system.actor.server.persistence.impl.FreeMarker;

@RunWith(Arquillian.class)
public class BusinessIT02 extends AbstractObject {

    @Deployment
    public static Archive<?> buildArchive() {
        WebArchive archive = ShrinkWrap.create(WebArchive.class, "acteur-business.war");
        archive.addAsResource("META-INF/beans.xml");
        archive.addAsResource("META-INF/MANIFEST.MF");
        archive.addAsResource("memory/persistence.xml","META-INF/persistence.xml");
        archive.addAsResource("memory/data.sql","META-INF/data.sql");
        archive.addPackages(Boolean.TRUE, ApplicationScopeLifeCycleListener.class.getPackage());
        archive.addPackages(Boolean.TRUE, AbstractUnitTest.class.getPackage());
        archive.addAsLibraries(Maven.resolver().loadPomFromFile("pom.xml").importCompileAndRuntimeDependencies().resolve().withTransitivity().asFile());
        archive.addAsLibraries(Maven.resolver().loadPomFromFile("pom.xml").importTestDependencies().resolve().withTransitivity().asFile());
        return archive;
    }

    @Test
	public void request_workflow_accept() throws Exception{
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_ENABLE, Boolean.TRUE);
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_HOST, "smtp.gmail.com");
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_PORT, 587);
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_AUTHENTICATION_REQUIRED, Boolean.TRUE);
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_SECURED_CONNECTION_REQUIRED, Boolean.TRUE);
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_AUTHENTICATION_CREDENTIALS_USER_IDENTIFIER, "sigobe.dgbf@gmail.com");
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_AUTHENTICATION_CREDENTIALS_USER_SECRET, "budget@2020");
		
		VariableHelper.write(FreeMarker.VARIABLE_NAME_REQUEST_READ_PAGE_URL, "http://siibtest.dgbf.ci/public/request/read.jsf");
		
		EntityCreator.getInstance().createMany(new RequestStatus().setCode(RequestStatus.CODE_INITIALIZED).setName("1")
				,new RequestStatus().setCode(RequestStatus.CODE_SUBMITTED).setName("1"),new RequestStatus().setCode(RequestStatus.CODE_ACCEPTED).setName("1")
				,new RequestStatus().setCode(RequestStatus.CODE_REJECTED).setName("1"));
		
		EntityCreator.getInstance().createMany(new IdentificationForm().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new RequestType().setCode("1").setFormFromIdentifier("1").setName("1"));
		
		EntityCreator.getInstance().createMany(new FunctionType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Function().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Function().setCode("2").setName("2").setTypeFromIdentifier("1"),new Function().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ScopeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Scope().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Scope().setCode("2").setName("2").setTypeFromIdentifier("1"),new Scope().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ScopeFunction().setCode("1").setName("1").setScopeFromIdentifier("1").setFunctionFromIdentifier("1")
				,new ScopeFunction().setCode("2").setName("2").setScopeFromIdentifier("2").setFunctionFromIdentifier("1")
				,new ScopeFunction().setCode("3").setName("3").setScopeFromIdentifier("3").setFunctionFromIdentifier("1"));
		
		Request request = new Request();
		request.setTypeFromIdentifier("1").setElectronicMailAddress("kycdev@gmail.com");
		request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "1"),EntityFinder.getInstance().find(ScopeFunction.class, "2")));
		__inject__(RequestBusiness.class).initialize(request);		
		
		//TimeHelper.pause(1000l * 30);
		
		String identifier = request.getIdentifier();
		request = RequestQuerier.getInstance().readByIdentifierForEdit(identifier);
		assertThat(request.getBudgetariesScopeFunctions()).isNotNull();
		assertThat(request.getBudgetariesScopeFunctions().size()).isEqualTo(2);		
		request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "2")));
		__inject__(RequestBusiness.class).record(request);
		
		request = RequestQuerier.getInstance().readByIdentifierForEdit(identifier);
		assertThat(request.getBudgetariesScopeFunctions()).isNotNull();
		assertThat(request.getBudgetariesScopeFunctions().size()).isEqualTo(1);
		
		request = EntityFinder.getInstance().find(Request.class, identifier);
		__inject__(RequestBusiness.class).submit(request);
		
		request = EntityFinder.getInstance().find(Request.class, identifier);
		__inject__(RequestBusiness.class).accept(request);
	}
	
	@Test
	public void scopeFunction_createFromAllScopesFromAllFunctions() throws Exception{
		EntityCreator.getInstance().createMany(new ScopeType().setCode(ScopeType.CODE_SECTION),new ScopeType().setCode(ScopeType.CODE_USB)
				,new ScopeType().setCode(ScopeType.CODE_UA),new ScopeType().setCode(ScopeType.CODE_ACTION),new FunctionType().setCode("BUDGETAIRE"));
		EntityCreator.getInstance().createMany(
				new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setName("Gestionnaire de crédits").setTypeFromIdentifier("BUDGETAIRE")
				,new Function().setCode("ORDP").setName("Ordonnateur principal").setTypeFromIdentifier("BUDGETAIRE")
				,new Function().setCode("ORD").setName("Ordonnateur").setTypeFromIdentifier("BUDGETAIRE")
				,new Function().setCode("CF").setName("Contrôleur financier").setTypeFromIdentifier("BUDGETAIRE")
				,new Function().setCode("CPT").setName("Comptable").setTypeFromIdentifier("BUDGETAIRE"));
		EntityCreator.getInstance().createMany(
				new ScopeTypeFunction().setScopeTypeFromIdentifier(ScopeType.CODE_SECTION).setFunctionFromIdentifier("CF")
				//,new ScopeTypeFunction().setScopeTypeFromIdentifier(ScopeType.CODE_SECTION).setFunctionFromIdentifier("ORDP")
				,new ScopeTypeFunction().setScopeTypeFromIdentifier(ScopeType.CODE_USB).setFunctionFromIdentifier("ORD")
				,new ScopeTypeFunction().setScopeTypeFromIdentifier(ScopeType.CODE_UA).setFunctionFromIdentifier(Function.CODE_CREDIT_MANAGER_HOLDER)
				);
		
		Long count = __inject__(ScopeFunctionPersistence.class).count();
		__inject__(ScopeFunctionBusiness.class).deriveAll();
		assertThat(__inject__(ScopeFunctionPersistence.class).count()).isEqualTo(count);
		
		EntityCreator.getInstance().createMany(
				new Scope().setCode("101").setName("Réprésentation Nationale").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("327").setName("Ministère du budget").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("22086").setName("Programme budget").setTypeFromIdentifier(ScopeType.CODE_USB)
				,new Scope().setCode("11025124").setName("Direction des traitements").setTypeFromIdentifier(ScopeType.CODE_UA)
				,new Scope().setCode("1102512401").setName("Action 01").setTypeFromIdentifier(ScopeType.CODE_ACTION)
		);
		
		__inject__(ScopeFunctionBusiness.class).deriveAll();
		assertThat(__inject__(ScopeFunctionPersistence.class).count()).isEqualTo(count+4);
		
		__inject__(ScopeFunctionBusiness.class).deriveAll();
		assertThat(__inject__(ScopeFunctionPersistence.class).count()).isEqualTo(count+4);
		
		__inject__(ScopeFunctionBusiness.class).codifyAll();
		assertThat(__inject__(ScopeFunctionPersistence.class).count()).isEqualTo(count+4);
		
		__inject__(ScopeFunctionBusiness.class).codifyAll();
		assertThat(__inject__(ScopeFunctionPersistence.class).count()).isEqualTo(count+4);
	}
	
	@Test
	public void scopeFunction_create_update() throws Exception{
		EntityCreator.getInstance().createMany(new ScopeType().setCode(ScopeType.CODE_SECTION),new ScopeType().setCode(ScopeType.CODE_USB)
				,new ScopeType().setCode(ScopeType.CODE_UA),new ScopeType().setCode(ScopeType.CODE_ACTION),new FunctionType().setCode("BUDGETAIRE"));
		EntityCreator.getInstance().createMany(
				new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setName("Gestionnaire de crédits").setTypeFromIdentifier("BUDGETAIRE")
				,new Function().setCode("ORDP").setName("Ordonnateur principal").setTypeFromIdentifier("BUDGETAIRE")
				,new Function().setCode("ORD").setName("Ordonnateur").setTypeFromIdentifier("BUDGETAIRE")
				,new Function().setCode("CF").setName("Contrôleur financier").setTypeFromIdentifier("BUDGETAIRE")
				,new Function().setCode("CPT").setName("Comptable").setTypeFromIdentifier("BUDGETAIRE"));
		EntityCreator.getInstance().createMany(
				new ScopeTypeFunction().setScopeTypeFromIdentifier(ScopeType.CODE_SECTION).setFunctionFromIdentifier("CF")
				//,new ScopeTypeFunction().setScopeTypeFromIdentifier(ScopeType.CODE_SECTION).setFunctionFromIdentifier("ORDP")
				,new ScopeTypeFunction().setScopeTypeFromIdentifier(ScopeType.CODE_USB).setFunctionFromIdentifier("ORD")
				,new ScopeTypeFunction().setScopeTypeFromIdentifier(ScopeType.CODE_UA).setFunctionFromIdentifier(Function.CODE_CREDIT_MANAGER_HOLDER)
				);
		
		EntityCreator.getInstance().createMany(
				new Scope().setCode("101").setName("Réprésentation Nationale").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("327").setName("Ministère du budget").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("22086").setName("Programme budget").setTypeFromIdentifier(ScopeType.CODE_USB)
				,new Scope().setCode("11025124").setName("Direction des traitements").setTypeFromIdentifier(ScopeType.CODE_UA)
				,new Scope().setCode("1102512401").setName("Action 01").setTypeFromIdentifier(ScopeType.CODE_ACTION)
		);
		
		__inject__(ScopeFunctionBusiness.class).create(new ScopeFunction().setScopeFromIdentifier("101").setFunctionFromIdentifier(Function.CODE_CREDIT_MANAGER_HOLDER).setShared(null));
		ScopeFunction scopeFunction = __inject__(ScopeFunctionPersistence.class).readByBusinessIdentifier("GC101");
		assertThat(scopeFunction).isNotNull();
		
		scopeFunction.setFunctionFromIdentifier("CF");
		__inject__(ScopeFunctionBusiness.class).update(scopeFunction);
		scopeFunction = __inject__(ScopeFunctionPersistence.class).readByBusinessIdentifier("CF101");
		assertThat(scopeFunction).isNotNull();
		
		scopeFunction = __inject__(ScopeFunctionPersistence.class).readByBusinessIdentifier("GC101");
		assertThat(scopeFunction).isNull();
	}
	
	@Test
	public void actor_create() throws Exception{
		EntityCreator.getInstance().createMany(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new FunctionType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Function().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Function().setCode("2").setName("2").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfileType().setCode(ProfileType.CODE_SYSTEME).setName("1")
				,new ProfileType().setCode(ProfileType.CODE_UTILISATEUR).setName("2"));
		EntityCreator.getInstance().createMany(new Profile().setCode("1").setName("1").setTypeFromIdentifier(ProfileType.CODE_SYSTEME)
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier(ProfileType.CODE_SYSTEME)
				,new Profile().setCode("3").setName("3").setTypeFromIdentifier(ProfileType.CODE_SYSTEME));
		
		EntityCreator.getInstance().createMany(new ProfileFunction().setProfileFromIdentifier("1").setFunctionFromIdentifier("1")
				,new ProfileFunction().setProfileFromIdentifier("2").setFunctionFromIdentifier("2"),new ProfileFunction().setProfileFromIdentifier("3").setFunctionFromIdentifier("2"));
		
		EntityCreator.getInstance().createMany(new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("1")
				,new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("3"),new ProfilePrivilege().setProfileFromIdentifier("2").setPrivilegeFromIdentifier("2"));
		
		Long profileCount = __inject__(ProfilePersistence.class).count();
		Long actorProfileCount = __inject__(ActorProfilePersistence.class).count();
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		__inject__(ActorBusiness.class).create(new Actor().setFirstName("yao").setLastNames("yves").setElectronicMailAddress("a@m.com")
				.addFunctionsByIdentifiers("1").setKeycloakUserCreatable(Boolean.FALSE));
		assertThat(__inject__(ProfilePersistence.class).count()).isEqualTo(profileCount);
		assertThat(__inject__(ActorProfilePersistence.class).count()).isEqualTo(actorProfileCount);
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount);
	}
	
	@Test
	public void actor_createActorScope_section() throws Exception{
		EntityCreator.getInstance().createMany(new ScopeType().setCode(ScopeType.CODE_SECTION),new ScopeType().setCode(ScopeType.CODE_USB)
				,new ScopeType().setCode(ScopeType.CODE_ACTION));
		
		EntityCreator.getInstance().createMany(
				new Scope().setCode("101").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("102").setTypeFromIdentifier(ScopeType.CODE_SECTION)
			);
		
		__inject__(ActorBusiness.class).createMany(List.of(
				new Actor().setIdentifier("u01").setCode("u01").setFirstName("yao").setLastNames("yves").setElectronicMailAddress("u01@m.com").setKeycloakUserCreatable(Boolean.FALSE)
					.setEmailSendableAfterCreation(Boolean.FALSE)
				,new Actor().setIdentifier("u02").setCode("u02").setFirstName("yao").setLastNames("yves").setElectronicMailAddress("u02@m.com").setKeycloakUserCreatable(Boolean.FALSE)
					.setEmailSendableAfterCreation(Boolean.FALSE)
			));
		
		Long actorScopeCount = __inject__(ActorScopePersistence.class).count();
		__inject__(ActorScopeBusiness.class).createByActorIdentifierByScopesIdentifier("u01", "101");
		assertThat(__inject__(ActorScopePersistence.class).count()).isEqualTo(actorScopeCount+1);
		ActorScope actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode("u01", "101");
		String identifier = actorScope.getIdentifier();
		assertThat(actorScope.getVisible()).isNull();
		
		__inject__(ActorScopeBusiness.class).createByActorIdentifierByScopesIdentifier("u01", "101");
		assertThat(__inject__(ActorScopePersistence.class).count()).isEqualTo(actorScopeCount+1);
		actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode("u01", "101");
		assertThat(actorScope.getIdentifier()).isEqualTo(identifier);
		assertThat(actorScope.getVisible()).isNull();
	}
	
	@Test
	public void actor_createActorScope_usb() throws Exception{
		EntityCreator.getInstance().createMany(new ScopeType().setCode(ScopeType.CODE_SECTION),new ScopeType().setCode(ScopeType.CODE_USB)
				,new ScopeType().setCode(ScopeType.CODE_ACTION));
		
		EntityCreator.getInstance().createMany(
				new Scope().setCode("101").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("102").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("101P01").setTypeFromIdentifier(ScopeType.CODE_USB)
				,new Scope().setCode("102P01").setTypeFromIdentifier(ScopeType.CODE_USB)
			);
		
		EntityCreator.getInstance().createMany(
				new Section().setCode("101")
				,new Section().setCode("102")
			);
		
		EntityCreator.getInstance().createMany(
				new BudgetSpecializationUnit().setCode("101P01").setSectionFromIdentifier("101").setSectionCodeName("101")
				,new BudgetSpecializationUnit().setCode("102P01").setSectionFromIdentifier("102").setSectionCodeName("102")
			);
		
		__inject__(ActorBusiness.class).createMany(List.of(
				new Actor().setIdentifier("u01").setCode("u01").setFirstName("yao").setLastNames("yves").setElectronicMailAddress("u01@m.com").setKeycloakUserCreatable(Boolean.FALSE)
					.setEmailSendableAfterCreation(Boolean.FALSE)
				,new Actor().setIdentifier("u02").setCode("u02").setFirstName("yao").setLastNames("yves").setElectronicMailAddress("u02@m.com").setKeycloakUserCreatable(Boolean.FALSE)
					.setEmailSendableAfterCreation(Boolean.FALSE)
			));
		
		Long actorScopeCount = __inject__(ActorScopePersistence.class).count();
		__inject__(ActorScopeBusiness.class).createByActorIdentifierByScopesIdentifier("u01", "101P01");
		assertThat(__inject__(ActorScopePersistence.class).count()).isEqualTo(actorScopeCount+1);
		ActorScope actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode("u01", "101P01");
		String usbIdentifier = actorScope.getIdentifier();
		assertThat(actorScope.getVisible()).isNull();
		assertThat(ScopeOfTypeSectionQuerier.getInstance().isVisible("101", "u01")).as("section 101 is not visible").isTrue();
		assertThat(ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().isVisible("101P01", "u01")).as("usb 101P01 is not visible").isTrue();
		
		__inject__(ActorScopeBusiness.class).createByActorIdentifierByScopesIdentifier("u01", "101P01");
		assertThat(__inject__(ActorScopePersistence.class).count()).isEqualTo(actorScopeCount+1);
		actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode("u01", "101P01");
		assertThat(actorScope.getIdentifier()).isEqualTo(usbIdentifier);
		assertThat(actorScope.getVisible()).isNull();
		
		__inject__(ActorScopeBusiness.class).createByActorIdentifierByScopesIdentifier("u01", "101");
		assertThat(__inject__(ActorScopePersistence.class).count()).isEqualTo(actorScopeCount+1);
		actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode("u01", "101P01");
		assertThat(actorScope.getIdentifier()).isEqualTo(usbIdentifier);
		assertThat(actorScope.getVisible()).isNull();
		actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode("u01", "101");
		assertThat(actorScope).isNull();
		
		__inject__(ActorScopeBusiness.class).createByActorIdentifierByScopesIdentifier("u01", "102");
		assertThat(__inject__(ActorScopePersistence.class).count()).isEqualTo(actorScopeCount+2);
		//actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode("u01", "102P01");
		//assertThat(actorScope.getIdentifier()).isEqualTo(usbIdentifier);
		//assertThat(actorScope.getVisible()).isNull();
		actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode("u01", "102");
		assertThat(actorScope).isNotNull();
	}
	
	@Test
	public void accountRequest_validate() throws Exception{
		EntityCreator.getInstance().createMany(new ProfileType().setCode(ProfileType.CODE_UTILISATEUR).setName("2"));
		
		__inject__(AccountRequestBusiness.class).record(List.of(
				new AccountRequest().setIdentifier("a").setFirstName("a").setLastNames("a").setElectronicMailAddress("a@m.com")
				,new AccountRequest().setIdentifier("b").setFirstName("b").setLastNames("b").setElectronicMailAddress("b@m.com")
				,new AccountRequest().setIdentifier("c").setFirstName("c").setLastNames("c").setElectronicMailAddress("c@m.com")));
			
		assertThat(__inject__(IdentityPersistence.class).count()).isEqualTo(3l);
		assertThat(__inject__(AccountRequestPersistence.class).count()).isEqualTo(3l);
		assertThat(__inject__(ActorPersistence.class).count()).isEqualTo(0l);
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(0l);
		
		__inject__(AccountRequestBusiness.class).accept(__inject__(AccountRequestPersistence.class).readBySystemIdentifier("a"));
		
		assertThat(__inject__(IdentityPersistence.class).count()).isEqualTo(3l);
		assertThat(__inject__(AccountRequestPersistence.class).count()).isEqualTo(2l);
		assertThat(__inject__(ActorPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(0l);
		__inject__(AccountRequestBusiness.class).reject(__inject__(AccountRequestPersistence.class).readBySystemIdentifier("b").setRejectReason("RAS"));
		
		assertThat(__inject__(IdentityPersistence.class).count()).isEqualTo(2l);
		assertThat(__inject__(AccountRequestPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(ActorPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(1l);
		
		__inject__(AccountRequestBusiness.class).delete(__inject__(AccountRequestPersistence.class).readBySystemIdentifier("c"));
		
		assertThat(__inject__(IdentityPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(AccountRequestPersistence.class).count()).isEqualTo(0l);
		assertThat(__inject__(ActorPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(1l);
	}
	
	@Test
	public void rejectedAccountRequest_create() throws Exception{		
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(0l);
		__inject__(RejectedAccountRequestBusiness.class).createMany(List.of(new RejectedAccountRequest().setFirstName("a").setLastNames("a")
				.setElectronicMailAddress("a@m.com").setDate(LocalDateTime.now()).setRequestDate(LocalDateTime.now())));			
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(1l);
	}
	
	@Test
	public void actor_editProfiles() throws Exception{
		createData();
		EntityCreator.getInstance().createMany(new Profile().setCode("P1").setName("1").setTypeFromIdentifier(ProfileType.CODE_UTILISATEUR));
		
		Long actorProfileCount = __inject__(ActorProfilePersistence.class).count();
		assertThat(actorProfileCount).isEqualTo(0l);
		assertThat(ProfileQuerier.getInstance().readByActorsCodes(List.of("admin"))).isNull();		
		__inject__(ActorBusiness.class).createProfiles(List.of(__inject__(ActorPersistence.class).readByBusinessIdentifier("admin"))
				,List.of(__inject__(ProfilePersistence.class).readByBusinessIdentifier("P1")));
		assertThat(__inject__(ActorProfilePersistence.class).count()).isEqualTo(actorProfileCount+1);
		assertThat(ProfileQuerier.getInstance().readByActorsCodes(List.of("admin")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("P1");
		
		__inject__(ActorBusiness.class).deleteProfiles(List.of(__inject__(ActorPersistence.class).readByBusinessIdentifier("admin"))
				,List.of(__inject__(ProfilePersistence.class).readByBusinessIdentifier("P1")));
		assertThat(ProfileQuerier.getInstance().readByActorsCodes(List.of("admin"))).isNull();
	}
	
	@Test
	public void profile_editPrivileges() throws Exception{
		createData();
		EntityCreator.getInstance().createMany(new Profile().setCode("P1").setName("1").setTypeFromIdentifier(ProfileType.CODE_UTILISATEUR));
		EntityCreator.getInstance().createMany(
				new Privilege().setCode("M1").setTypeFromIdentifier(PrivilegeType.CODE_MODULE)
				);
		EntityCreator.getInstance().createMany(
				new Privilege().setCode("S1").setTypeFromIdentifier(PrivilegeType.CODE_SERVICE).setParentIdentifier("M1")
				,new Privilege().setCode("S2").setTypeFromIdentifier(PrivilegeType.CODE_SERVICE).setParentIdentifier("M1")
				);
		
		EntityCreator.getInstance().createMany(
				new Privilege().setCode("ME1").setTypeFromIdentifier(PrivilegeType.CODE_MENU).setParentIdentifier("S1")
				,new Privilege().setCode("ME1.1").setTypeFromIdentifier(PrivilegeType.CODE_MENU).setParentIdentifier("ME1")
				,new Privilege().setCode("ME1.2").setTypeFromIdentifier(PrivilegeType.CODE_MENU).setParentIdentifier("ME1")
				,new Privilege().setCode("ME2").setTypeFromIdentifier(PrivilegeType.CODE_MENU).setParentIdentifier("S1")
				,new Privilege().setCode("ME2.1").setTypeFromIdentifier(PrivilegeType.CODE_MENU).setParentIdentifier("ME2")
				,new Privilege().setCode("ME2.2").setTypeFromIdentifier(PrivilegeType.CODE_MENU).setParentIdentifier("ME2")
				,new Privilege().setCode("ME2.3").setTypeFromIdentifier(PrivilegeType.CODE_MENU).setParentIdentifier("ME2")
				);
		
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(0l);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("admin"))).isNull();		
		__inject__(ProfileBusiness.class).savePrivileges(List.of(__inject__(ProfilePersistence.class).readByBusinessIdentifier("P1"))
				,__inject__(PrivilegePersistence.class).readByBusinessIdentifiers(CollectionHelper.cast(Object.class, List.of("M1","S1","ME1","ME1.1"))),null);
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+1);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("P1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactlyInAnyOrder("ME1.1");
		assertThat(PrivilegeQuerier.getInstance().readVisibleByProfilesCodes(List.of("P1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactlyInAnyOrder("M1","S1","ME1","ME1.1");
		
		__inject__(ProfileBusiness.class).savePrivileges(List.of(__inject__(ProfilePersistence.class).readByBusinessIdentifier("P1"))
				,null,List.of(__inject__(PrivilegePersistence.class).readByBusinessIdentifier("ME1.1")));
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("P1"))).isNull();
		
		__inject__(ProfileBusiness.class).savePrivileges(List.of(__inject__(ProfilePersistence.class).readByBusinessIdentifier("P1"))
				,__inject__(PrivilegePersistence.class).readByBusinessIdentifiers(CollectionHelper.cast(Object.class, List.of("M1","S1","ME1","ME1.1","ME1.2"))),null);
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+2);
		__inject__(ProfileBusiness.class).savePrivileges(List.of(__inject__(ProfilePersistence.class).readByBusinessIdentifier("P1"))
				,null,List.of(__inject__(PrivilegePersistence.class).readByBusinessIdentifier("ME1.1")));
		
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("P1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactlyInAnyOrder("ME1.2");
		assertThat(PrivilegeQuerier.getInstance().readVisibleByProfilesCodes(List.of("P1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactlyInAnyOrder("M1","S1","ME1","ME1.2");
	}
	
	@Test
	public void profilePrivilege_createFromPrivileges() throws Exception{
		EntityCreator.getInstance().createMany(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfileType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Profile().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier("1"),new Profile().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(0l);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1"))).isNull();
		__inject__(ProfilePrivilegeBusiness.class).createFromPrivileges(__inject__(ProfilePersistence.class).readBySystemIdentifier("1")
				, List.of(__inject__(PrivilegePersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+1);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1");
		
		profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(1l);
		__inject__(ProfilePrivilegeBusiness.class).createFromPrivileges(__inject__(ProfilePersistence.class).readBySystemIdentifier("1")
				, List.of(__inject__(PrivilegePersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1");
	}
	
	@Test
	public void profilePrivilege_createFromProfiles() throws Exception{
		EntityCreator.getInstance().createMany(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfileType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Profile().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier("1"),new Profile().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("2"));
		
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(1l);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2"))).isNull();
		__inject__(ProfilePrivilegeBusiness.class).createFromProfiles(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(ProfilePersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+1);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
		
		profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(2l);
		__inject__(ProfilePrivilegeBusiness.class).createFromProfiles(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(ProfilePersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
	}
	
	@Test
	public void profilePrivilege_createFromFunctions() throws Exception{
		EntityCreator.getInstance().createMany(new FunctionType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Function().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Function().setCode("2").setName("2").setTypeFromIdentifier("1"),new Function().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1")
				,new Privilege().setCode("4").setName("4").setTypeFromIdentifier("1"),new Privilege().setCode("5").setName("5").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfileType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Profile().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier("1"),new Profile().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("1")
				,new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("2")
				,new ProfilePrivilege().setProfileFromIdentifier("3").setPrivilegeFromIdentifier("3")
				,new ProfilePrivilege().setProfileFromIdentifier("3").setPrivilegeFromIdentifier("4")
				,new ProfilePrivilege().setProfileFromIdentifier("3").setPrivilegeFromIdentifier("5"));
		
		EntityCreator.getInstance().createMany(new ProfileFunction().setProfileFromIdentifier("1").setFunctionFromIdentifier("1")
				,new ProfileFunction().setProfileFromIdentifier("2").setFunctionFromIdentifier("2")
				,new ProfileFunction().setProfileFromIdentifier("3").setFunctionFromIdentifier("3"));
		
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(5l);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2"))).isNull();
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("3")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("3","4","5");
		
		__inject__(ProfilePrivilegeBusiness.class).createFromFunctions(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(FunctionPersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+2);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		
		profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		__inject__(ProfilePrivilegeBusiness.class).createFromFunctions(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(FunctionPersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		
		__inject__(ProfilePrivilegeBusiness.class).createFromFunctions(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(FunctionPersistence.class).readBySystemIdentifier("2")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		
		__inject__(ProfilePrivilegeBusiness.class).createFromFunctions(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(FunctionPersistence.class).readBySystemIdentifier("3")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+3);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2","3","4","5");
	}

	@Test
	public void actor_deleteVisibleSections(){
		createData();		
		assertDeleteVisibleSections("admin", "s1","s2","s3");
	}
	
	@Test
	public void actor_deleteVisibleBudgetSpecializationUnits(){
		createData();		
		assertDeleteVisibleBudgetSpecializationUnits("admin", "usb1","usb2","usb3","usb4","usb5");
	}
	
	@Test
	public void actor_deleteVisibleActivities(){
		createData();		
		assertDeleteVisibleActivities("admin", "a1","a2","a3","a4","a5","a6");
	}
	
	/**/
	
	protected void createData() {
		EntityCreator.getInstance().createMany(
				new ScopeType().setCode(ScopeType.CODE_SECTION).setOrderNumber((byte)1)
				,new ScopeType().setCode(ScopeType.CODE_USB).setOrderNumber((byte)2)
				,new ScopeType().setCode(ScopeType.CODE_ACTIVITE).setOrderNumber((byte)3)
				,new ScopeType().setCode(ScopeType.CODE_IMPUTATION).setOrderNumber((byte)4)
				,new ScopeType().setCode(ScopeType.CODE_UA).setOrderNumber((byte)5)
				,new ProfileType().setCode(ProfileType.CODE_SYSTEME)
				,new ProfileType().setCode(ProfileType.CODE_UTILISATEUR)
				,new PrivilegeType().setCode(PrivilegeType.CODE_MODULE).setOrderNumber((byte)0)
				,new PrivilegeType().setCode(PrivilegeType.CODE_SERVICE).setOrderNumber((byte)0)
				,new PrivilegeType().setCode(PrivilegeType.CODE_MENU).setOrderNumber((byte)0)
				,new PrivilegeType().setCode(PrivilegeType.CODE_ACTION).setOrderNumber((byte)0)
				);
				
		//Sections
		createSection("s1");
		createSection("s2");
		createSection("s3");
		
		//USBs
		createBudgetSpecializationUnit("usb1", "s1");
		createBudgetSpecializationUnit("usb2", "s1");
		createBudgetSpecializationUnit("usb3", "s1");		
		createBudgetSpecializationUnit("usb4", "s3");
		createBudgetSpecializationUnit("usb5", "s3");
		
		//Activities
		createActivity("a1", "usb1");
		createActivity("a2", "usb1");
		createActivity("a3", "usb2");
		createActivity("a4", "usb2");
		createActivity("a5", "usb2");
		createActivity("a6", "usb5");
		
		//Imputations
		createImputation("i1", "a1");
		
		//UAs
		createAdministrativeUnit("ua1", "s1");
		createAdministrativeUnit("ua2", "s1");
		createAdministrativeUnit("ua3", "s2");
		createAdministrativeUnit("ua4", "s2");
		createAdministrativeUnit("ua5", "s2");
		createAdministrativeUnit("ua6", "s3");
		createAdministrativeUnit("ua7", "s3");
		createAdministrativeUnit("ua8", "s3");
		createAdministrativeUnit("ua9", "s3");
		
		//Actors
		createActor("inconnu");
		createActor("admin","s1","s2","s3");
		
		createActor("section_manager_1","s1");
		createActor("section_manager_2","s2");
		createActor("section_manager_3","s3");
		createActor("section_manager_4","s1","s3");
		
		createActor("usb_manager_1","usb1");
		createActor("usb_manager_2","usb2");
		createActor("usb_manager_3","usb3","usb4","usb5");
		
		createActor("activity_manager_1","a1");
		createActor("activity_manager_2","a2","a3","a4");
		createActor("activity_manager_3","a5","a6");
		
		createActor("ua_manager_1","ua1","ua3");
		createActor("ua_manager_2","ua2");
		createActor("ua_manager_3","ua6","ua8","ua9");
	}
	
	private void createSection(String code) {
		Scope sectionScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_SECTION);
		EntityCreator.getInstance().createMany(sectionScope);
		Section section = new Section().setCode(sectionScope.getIdentifier());
		EntityCreator.getInstance().createMany(section);
	}
	
	private void createBudgetSpecializationUnit(String code,String sectionIdentifier) {
		Scope usbScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_USB);
		BudgetSpecializationUnit usb = new BudgetSpecializationUnit().setCode(usbScope.getCode()).setSectionFromIdentifier(sectionIdentifier); 
		usb.setSectionCodeName(usb.getSection().toString());
		EntityCreator.getInstance().createMany(usbScope,usb);
	}
	
	private void createActivity(String code,String budgetSpecializationUnitIdentifier) {
		Scope activityScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_ACTIVITE);
		Activity activity = new Activity().setCode(activityScope.getIdentifier()).setCode(activityScope.getCode())
				.setBudgetSpecializationUnitFromIdentifier(budgetSpecializationUnitIdentifier);
		activity.setSection(activity.getBudgetSpecializationUnit().getSection());
		activity.setSectionCodeName(activity.getSection().toString());
		activity.setBudgetSpecializationUnitCodeName(activity.getBudgetSpecializationUnit().toString());
		activity.setActionCodeName("");
		EntityCreator.getInstance().createMany(activityScope,activity);	
	}
	
	private void createImputation(String code,String activityIdentifier) {
		Scope imputationScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_IMPUTATION);
		Imputation imputation = new Imputation().setCode(code).setCode(code).setActivityFromIdentifier(activityIdentifier);
		imputation.setBudgetSpecializationUnit(imputation.getActivity().getBudgetSpecializationUnit());
		imputation.setSection(imputation.getBudgetSpecializationUnit().getSection());
		EntityCreator.getInstance().createMany(imputationScope,imputation);
	}
	
	private void createAdministrativeUnit(String code,String sectionIdentifier) {
		Scope administrativeUnitScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_UA);
		AdministrativeUnit administrativeUnit = new AdministrativeUnit().setCode(administrativeUnitScope.getCode()).setSectionFromIdentifier(sectionIdentifier);
		EntityCreator.getInstance().createMany(administrativeUnitScope,administrativeUnit);
	}
	
	private void createActor(String code,String...scopesIdentifiers) {
		Actor actor = new Actor().setCode(code).setFirstName(code).setLastNames(code).setElectronicMailAddress(code);
		__inject__(ActorBusiness.class).create(actor);		
		createActorScopes(actor, scopesIdentifiers);
	}
	
	private void createActorScopes(Actor actor,String...scopesIdentifiers) {
		if(ArrayHelper.isNotEmpty(scopesIdentifiers))
			for(String scopesIdentifier : scopesIdentifiers)
				__inject__(ActorScopeBusiness.class).createByActorByScopes(actor, List.of(__inject__(ScopePersistence.class).readByBusinessIdentifier(scopesIdentifier))); 
	}
	
	protected void createActorScopes(String code,String...scopesIdentifiers) {
		createActorScopes(__inject__(ActorPersistence.class).readByBusinessIdentifier(code), scopesIdentifiers);
	}
	
	private void assertVisibleSectionsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeSectionQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertVisibleBudgetSpecializationUnitsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertVisibleActivitiesByFilter(String actorCode,String code,String name,String...expectedCodes) {
		Collection<Scope> scopes = ScopeOfTypeActivityQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name));
		assertScopesExactly(scopes,expectedCodes);
	}
	
	private void assertDeleteVisibleSections(String actorCode,String...codes) {
		String code = codes[0];
		String[] otherCodes = ArrayUtils.subarray(codes, 1, codes.length);
		assertVisibleSectionsByFilter(actorCode, null, null, codes);
		ActorScope actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorCode, code);
		assertThat(actorScope).isNotNull();
		
		for(Integer count = 0; count < 10; count = count + 1) {
			deleteActorScope(actorCode, code);
			actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorCode, code);
			assertThat(actorScope).isNull();
			assertVisibleSectionsByFilter(actorCode, null, null, otherCodes);
			
			createActorScopes(actorCode, code);
			actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorCode, code);
			assertThat(actorScope).isNotNull();
			assertThat(actorScope.getVisible()).isNull();
			assertVisibleSectionsByFilter(actorCode, null, null, codes);
		}
	}
	
	private void assertDeleteVisibleBudgetSpecializationUnits(String actorCode,String...codes) {
		String code = codes[0];
		String[] otherCodes = ArrayUtils.subarray(codes, 1, codes.length);
		assertVisibleBudgetSpecializationUnitsByFilter(actorCode, null, null, codes);
		ActorScope actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorCode, code);
		assertThat(actorScope).isNull();
		
		for(Integer count = 0; count < 10; count = count + 1) {
			deleteActorScope(actorCode, code);
			actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorCode, code);
			assertThat(actorScope).isNotNull();
			assertThat(actorScope.getVisible()).isFalse();		
			assertVisibleBudgetSpecializationUnitsByFilter(actorCode, null, null, otherCodes);
			
			createActorScopes(actorCode, code);
			actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorCode, code);
			assertThat(actorScope).isNotNull();
			assertThat(actorScope.getVisible()).isNull();		
			assertVisibleBudgetSpecializationUnitsByFilter(actorCode, null, null, codes);
		}
	}
	
	private void assertDeleteVisibleActivities(String actorCode,String...codes) {
		String code = codes[0];
		String[] otherCodes = ArrayUtils.subarray(codes, 1, codes.length);
		assertVisibleActivitiesByFilter(actorCode, null, null, codes);
		ActorScope actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorCode, code);
		assertThat(actorScope).isNull();
		
		for(Integer count = 0; count < 10; count = count + 1) {
			deleteActorScope(actorCode, code);
			actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorCode, code);
			assertThat(actorScope).isNotNull();
			assertThat(actorScope.getVisible()).isFalse();		
			assertVisibleActivitiesByFilter(actorCode, null, null, otherCodes);
			
			createActorScopes(actorCode, code);
			actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorCode, code);
			assertThat(actorScope).isNotNull();
			assertThat(actorScope.getVisible()).isNull();		
			assertVisibleActivitiesByFilter(actorCode, null, null, codes);
		}
	}
	
	private void deleteActorScope(String actorCode,String scopeCode) {
		Actor actor = __inject__(ActorPersistence.class).readByBusinessIdentifier(actorCode);
		Scope scope = __inject__(ScopePersistence.class).readByBusinessIdentifier(scopeCode);
		__inject__(ActorScopeBusiness.class).deleteByActorByScopes(actor, List.of(scope));
	}
	
	private void assertScopesExactly(Collection<Scope> scopes,String...expectedCodes) {
		assertScopes(scopes, Boolean.TRUE, expectedCodes);
	}
	
	private void assertScopes(Collection<Scope> scopes,Boolean exactly,String...expectedCodes) {
		if(CollectionHelper.isEmpty(scopes)) {
			assertThat(ArrayHelper.isEmpty(expectedCodes)).as("No scopes found").isTrue();
		}else {
			if(exactly == null || Boolean.TRUE.equals(exactly))
				assertThat(scopes.stream().map(x -> x.getCode()).collect(Collectors.toList())).containsExactly(expectedCodes);
			else
				assertThat(scopes.stream().map(x -> x.getCode()).collect(Collectors.toList())).contains(expectedCodes);
		}
	}
}