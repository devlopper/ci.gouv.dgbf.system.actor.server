package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.ByteArrayOutputStream;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import javax.ejb.Schedule;
import javax.ejb.Stateless;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.file.FileType;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.EntityLifeCycleListener;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.mail.MailSender;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.report.ReportGetter;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;

@Stateless
public class RequestScopeFunctionBusinessImpl extends AbstractBusinessEntityImpl<RequestScopeFunction, RequestScopeFunctionPersistence> implements RequestScopeFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Schedule(hour = "*",minute = "*/5",persistent = false)
	public void updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiersAutomatically() {
		Collection<Object[]> arrays = EntityManagerGetter.getInstance().get().createNamedQuery(RequestScopeFunction.QUERY_READ_RELEASABLE).getResultList();
		if(CollectionHelper.isEmpty(arrays))
			return;
		for(Object[] array : arrays)
			try {
				updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(ValueHelper.defaultToIfBlank((String)array[1],"SYSTEME"), (String)array[0]);
			} catch (Exception exception) {
				LogHelper.log(exception, getClass());
			}
	}
	
	@Override @Transactional
	public TransactionResult updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers, String actorCode) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scope functions identifiers", scopeFunctionsIdentifiers);
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("actor code", actorCode);
		TransactionResult result = new TransactionResult().setName("Libération de poste").setTupleName("poste");
		Integer count = RequestScopeFunctionQuerier.getInstance().updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(scopeFunctionsIdentifiers,actorCode
				,"LIBERATION",EntityLifeCycleListener.Event.UPDATE.getValue(),LocalDateTime.now(), EntityManagerGetter.getInstance().get());
		result.incrementNumberOfUpdate(count == null ? 0l : count.longValue());
		result.log(RequestScopeFunctionBusinessImpl.class);
		return result;
	}

	@Override @Transactional
	public TransactionResult updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(String actorCode,String... scopeFunctionsIdentifiers) {
		return updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(ArrayHelper.isEmpty(scopeFunctionsIdentifiers) ? null : CollectionHelper.listOf(scopeFunctionsIdentifiers)
				, actorCode);
	}

	@Override
	public void notifySignatureSpecimen(Collection<String> identifiers) {
		if(CollectionHelper.isEmpty(identifiers))
			throw new RuntimeException("Identifiants des postes demandés obligatoires");
		Collection<Object[]> arrays = EntityManagerGetter.getInstance().get().createQuery("SELECT t.request.identifier,t.scopeFunction.identifier,t.scopeFunction.function.code,t.request.electronicMailAddress,t.scopeFunction.name"
				+ ",t.request.type.creditManagerSignatureSpecimenReportIdentifier,t.request.type.authorizingOfficerSignatureSpecimenReportIdentifier FROM RequestScopeFunction t WHERE t.identifier IN :identifiers")
				.setParameter("identifiers", identifiers).getResultList();
		Collection<String> unprocessableFunctionsCodesNames = arrays.stream().filter(array -> !Function.CODE_CREDIT_MANAGER_HOLDER.equals(array[2]) && !Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(array[2]))
				.map(array -> (String)array[2]+" "+array[4]).collect(Collectors.toSet());
		if(CollectionHelper.isNotEmpty(unprocessableFunctionsCodesNames))
			throw new RuntimeException(String.format("Les fonctions suivantes n'ont pas de spécimen de signature. %s", StringHelper.concatenate(unprocessableFunctionsCodesNames, ",")));		
		Collection<String> receivers = arrays.stream().map(array -> (String)array[3]).collect(Collectors.toSet());
		Map<String,Collection<Object[]>> map = new HashMap<String, Collection<Object[]>>();
		receivers.forEach(receiver -> {
			map.put(receiver, arrays.stream().filter(array -> receiver.equals(array[3])).collect(Collectors.toList()));
		});
		
		new Thread(new Runnable() {				
			@Override
			public void run() {
				for(String receiver : receivers) {
					try {
						org.cyk.utility.mail.Message message = new org.cyk.utility.mail.Message();
						message.setSubject("SIGOBE - Spécimen de signature");
						message.setBody("Ci joint les fichiers");
						message.addReceiversFromStrings(receiver);						
						for(Object[] array : map.get(receiver)) {
							String reportIdentifier = null;
							if(Function.CODE_CREDIT_MANAGER_HOLDER.equals(array[2]))
								reportIdentifier = (String) array[5];
							else if(Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(array[2]))
								reportIdentifier = (String) array[6];
							if(StringHelper.isBlank(reportIdentifier)) {
								LogHelper.logSevere(String.format("La fonction %s n'a pas de spécimen de signature identifié dans le système", array[2]), RequestScopeFunctionBusinessImpl.class);
								continue;
							}
							ByteArrayOutputStream byteArrayOutputStream = ReportGetter.getInstance().get(reportIdentifier,Map.of("identifiant",(String)array[0],"poste",(String)array[1]),FileType.PDF);
							if(byteArrayOutputStream == null) {
								LogHelper.logSevere(String.format("Le flux du spécimen de signature de %s|%s est null", array[0],array[1]), getClass());
								continue;
							}
							message.addAttachments(new org.cyk.utility.mail.Message.Attachment().setBytes(byteArrayOutputStream.toByteArray()).setExtension("pdf")
									.setName("specimen_de_signature_"+array[2]));
						}
						LogHelper.logInfo(String.format("%s fichier(s) joint(s) à envoyer", CollectionHelper.getSize(message.getAttachments())), getClass());
						MailSender.getInstance().send(message);
					} catch (Exception exception) {
						LogHelper.log(exception, getClass());
					}
				}
			}
		}).start();
	}

	@Override
	public void notifySignatureSpecimen(String... identifiers) {
		notifySignatureSpecimen(CollectionHelper.listOf(Boolean.TRUE,identifiers));
	}
}