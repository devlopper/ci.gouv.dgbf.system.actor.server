package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.marker.AuditableWhoDoneWhatWhen;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.audit.AuditReader;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public abstract class AbstractUnitTestMemory extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	protected static <T extends AuditableWhoDoneWhatWhen> void assertAudit(Class<T> klass,LocalDateTime fromDate,LocalDateTime toDate,QueryExecutorArguments queryExecutorArguments,Integer expectedCount) {
		if(queryExecutorArguments == null)
			queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addProcessableTransientFieldsNames(ScopeFunction.FIELD___AUDIT_WHEN_AS_STRING__);
		Collection<T> audits = AuditReader.getInstance().read(klass,new AuditReader.Arguments<T>().setQueryExecutorArguments(queryExecutorArguments)
				.setFromDate(fromDate).setToDate(toDate).setIsReadableByDates(Boolean.TRUE));
		
		System.out.println("AbstractUnitTestMemory.assertAudit() : "+fromDate+" - "+toDate+" : "+expectedCount);	
		if(CollectionHelper.isNotEmpty(audits))
			audits.forEach(audit -> {
				System.out.println(audit.get__auditWhenAsString__());
			});
					
		assertThat(CollectionHelper.getSize(audits)).isEqualTo(expectedCount);
		if(CollectionHelper.isEmpty(audits))
			return;
		audits.forEach(audit -> {
			assertThat(FieldHelper.readSystemIdentifier(audit)).as("identifier is null").isNotNull();
			assertThat(audit.get__auditWhenAsString__()).as("when as string is null").isNotNull();
		});
	}
	
	protected static <T extends AuditableWhoDoneWhatWhen> void assertAudit(Class<T> klass,LocalDateTime fromDate,LocalDateTime toDate,Integer expectedCount) {
		assertAudit(klass, fromDate, toDate, null, expectedCount);
	}
	
	protected static void addPause(List<LocalDateTime> dateTimes,Integer duration) {
		if(dateTimes == null || NumberHelper.isLessThanOrEqualZero(duration))
			return;
		LocalDateTime date = null;
		//if(dateTimes.isEmpty())
			date = LocalDateTime.now();
		//else
		//	date = CollectionHelper.getLast(dateTimes).plus(duration.longValue(),ChronoField.MILLI_OF_DAY.getBaseUnit());
		dateTimes.add(date);
		TimeHelper.pause(duration.longValue());
	}
	
	protected static void addPause(List<LocalDateTime> dateTimes) {
		addPause(dateTimes, 100);
	}
}