package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.business.server.EntityCreator;
import org.cyk.utility.business.server.EntityUpdater;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

@ApplicationScoped
public class ScopeTypeBusinessImpl extends AbstractBusinessEntityImpl<ScopeType,ScopeTypePersistence> implements ScopeTypeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
	private static void set(ScopeType scopeType,String code, String name,Byte orderNumber,Boolean requestable,String actorCode) {
		scopeType.setCode(code).setName(name).setOrderNumber(orderNumber).setRequestable(requestable);
	}
	
	public static TransactionResult create(String code, String name,Byte orderNumber,Boolean requestable,String actorCode,EntityManager entityManager) {
		ValidatorImpl.validateScopeType(code, name, orderNumber, requestable, actorCode, entityManager);
		TransactionResult transactionResult = new TransactionResult().setTupleName(ScopeType.LABEL).setName("Création "+ScopeType.LABEL);
		ScopeType scopeType = new ScopeType();
		set(scopeType, code, name, orderNumber, requestable, actorCode);
		EntityCreator.getInstance().create(new QueryExecutorArguments().setEntityManager(entityManager).setObjects(CollectionHelper.cast(Object.class, List.of(scopeType))));
		transactionResult.incrementNumberOfCreation(1l);
		transactionResult.log(ScopeTypeBusinessImpl.class);
		return transactionResult;
	}
	
	@Override @Transactional
	public TransactionResult create(String code, String name,Byte orderNumber,Boolean requestable,String actorCode) {
		return create(code, name,orderNumber,requestable,actorCode, EntityManagerGetter.getInstance().get());
	}
	
	public static TransactionResult update(String identifier,String code, String name,Byte orderNumber,Boolean requestable,String actorCode,EntityManager entityManager) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("identifiant",identifier);		
		ValidatorImpl.validateScopeType(code, name, orderNumber, requestable, actorCode, entityManager);
		TransactionResult transactionResult = new TransactionResult().setTupleName(ScopeType.LABEL).setName("Mise à jour "+ScopeType.LABEL);
		ScopeType scopeType = EntityFinder.getInstance().find(ScopeType.class, identifier);
		set(scopeType, code, name, orderNumber, requestable, actorCode);
		EntityUpdater.getInstance().update(new QueryExecutorArguments().setEntityManager(entityManager).setObjects(CollectionHelper.cast(Object.class, List.of(scopeType))));
		transactionResult.incrementNumberOfCreation(1l);
		transactionResult.log(ScopeTypeBusinessImpl.class);
		return transactionResult;
	}
	
	@Override @Transactional
	public TransactionResult update(String identifier,String code, String name,Byte orderNumber,Boolean requestable,String actorCode) {
		return update(identifier,code, name,orderNumber,requestable,actorCode, EntityManagerGetter.getInstance().get());
	}
	
	public static TransactionResult save(String identifier,String code, String name,Byte orderNumber,Boolean requestable,String actorCode,EntityManager entityManager) {
		if(StringHelper.isBlank(identifier))
			return create(code, name, orderNumber, requestable, actorCode,entityManager);
		return update(identifier, code, name, orderNumber, requestable, actorCode, entityManager);
	}
	
	@Override
	public TransactionResult save(String identifier, String code, String name, Byte orderNumber,Boolean requestable, String actorCode) {
		return save(identifier,code, name,orderNumber,requestable,actorCode, EntityManagerGetter.getInstance().get());
	}
	
	@Override
	public Collection<ScopeType> get(Boolean requestable,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(ScopeType.class)
				.filterIfNotNull(ScopeTypeQuerier.PARAMETER_NAME_REQUESTABLE,requestable)
				.page(pageable, firstTupleIndex, numberOfTuples);
		return EntityReader.getInstance().readMany(ScopeType.class, arguments);
	}
}